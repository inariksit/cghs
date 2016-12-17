module Parse ( parse
             ) where

import qualified Rule as R
import CG.Abs
import CG.Lex
import CG.Par
import CG.Print
import CG.ErrM

import Control.Monad.State.Lazy
import Data.Either
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Regex.PCRE

type Result = ([String], [[R.Rule]]) -- sections

parse :: String -> Result 
parse s = case pGrammar (CG.Par.myLexer s) of
  Bad err  -> error err
  Ok  tree -> let (rules,env) = runState (parseRules tree) emptyEnv
              in ( map (show.snd) (tagsets env) ++ map (show.snd) (templates env)
                 , rules )

--------------------------------------------------------------------------------


data Env = Env { rules ::[R.Rule]
               , tagsets :: [(String, R.TagSet)]
--               , inlineTagsets :: [R.TagSet] -- keep track of inline tag sets
               , templates :: [(String, R.Context)] }

emptyEnv = Env [] [] []

putRule :: R.Rule -> State Env ()
putRule x = modify $ \env -> env { rules = x : rules env }

putSet :: (String,R.TagSet) -> State Env ()
putSet x = modify $ \env -> env { tagsets = x : tagsets env }

putTempl :: (String,R.Context) -> State Env ()
putTempl x = modify $ \env -> env { templates = x : templates env }

getSet :: (Env -> [(String,a)]) -> String -> State Env a
getSet f nm = do env <- gets f
                 let set = fromMaybe 
                            (error $ "Tagset/template " ++ nm ++ " not defined!")
                            (lookup nm env) 
                 return set

--------------------------------------------------------------------------------

--this is awkward, TODO fix it
addDecl :: Def -> State Env (Either String R.Rule)
addDecl (SetDef s)   = do transSetDecl s >>= putSet
                          return (Left $ CG.Print.printTree s)
addDecl (TemplDef t) = do transTemplDecl t >>= putTempl
                          return (Left $ CG.Print.printTree t)
addDecl (RuleDef r)  = do rl <- transRule r
                          putRule rl
                          return (Right rl)

parseRules :: Grammar -> State Env [[R.Rule]]
parseRules (Sections secs) = mapM parseSection secs

parseSection :: Section -> State Env [R.Rule]
parseSection (Defs defs) =
 do putSet (bosString, R.bosSet) --in case the grammar doesn't specify boundaries 
    putSet (eosString, R.eosSet)
    rlsDefs <- mapM addDecl defs
    return (rights rlsDefs)

--------------------------------------------------------------------------------
-- Rule

transRule :: Rule -> State Env R.Rule
transRule x = case x of
  RemoveAlways nm sr tags  -> do let (name,subr) = (transName nm, transSubrTarget sr)
                                 target <- mapSubr subr `fmap` transTagSet tags
                                 return (R.R R.REMOVE name target always)
  RemoveIf nm sr tags _ cs -> do let (name,subr) = (transName nm, transSubrTarget sr)
                                 target <- mapSubr subr `fmap` transTagSet tags
                                 ctx <- mapM transCond cs
                                 return (R.R R.REMOVE name target (R.And ctx))
  SelectAlways nm sr tags  -> select `fmap` transRule (RemoveAlways nm sr tags)
  SelectIf nm sr tags x cs -> select `fmap` transRule (RemoveIf nm sr tags x cs)
  MatchLemma str rule -> undefined

  where
    always = R.And [R.Always]
    select rl = rl { R.rtype = R.SELECT}

    transName (MaybeName1 (Id nm)) = R.Name nm
    transName MaybeName2           = R.NoName



--------------------------------------------------------------------------------
-- Sets: BOS/EOS; simple names (Adj); syntactic tags (@OBJ); weird stuff (<;>)

showSetName :: SetName -> String
showSetName setname =
  case setname of
    SetName (UIdent name) -> name
    SetMeta (UIdent name) -> "<" ++ name ++ ">"
    SetSynt (UIdent name) -> "@" ++ name

bosString = ">>>"
eosString = "<<<"

transSetDecl :: SetDecl -> State Env (String, R.TagSet)
transSetDecl setdecl =
  case setdecl of 
    Set nm tagset -> (,) (showSetName nm) `fmap` transTagSet tagset
    List nm tags  -> do let tagLists = map transTag tags :: [TagList]
                        let setName = showSetName nm
                        return (setName, R.List (R.Or tagLists))

    BList -> return (bosString, R.bosSet)
    EList -> return (eosString, R.eosSet)


-- Templates: single or a list with ORs
transTemplDecl :: TemplDecl -> State Env (String, R.Context)
transTemplDecl templ = case templ of
  SingleTempl nm cond  -> (,) (showSetName nm) `fmap` transCond cond
  ListTempl   nm conds -> (,) (showSetName nm) `fmap` transCond (CondTemplInl conds)

--------------------------------------------------------------------------------
-- Tags and tagsets

type TagList = R.AndList R.Tag

toTag :: String -> R.Tag
toTag s = case s of
  ('"':'<':_) -> R.WF (strip 2 s)
  ('"':    _) -> R.Lem (strip 1 s)
  _           -> R.Tag s
  where 
    strip :: Int -> String -> String
    strip n = drop n . reverse . drop n . reverse

transTag :: Tag -> TagList
transTag tag = case tag of
  BOS -> R.And [R.BOS]
  EOS -> R.And [R.EOS]
  Lemma (Str s) -> R.And [toTag s]
  Tag (Id str)  -> R.And [toTag str]
  AND tags      -> R.And $ concatMap (R.getAndList.transTag) tags

  --TODO: case-insensitive lemma + regex
  LemmaCI foo   -> transTag (Lemma foo) 
  Regex foo     -> transTag (Lemma foo)
  -- /TODO


transTagSet :: TagSet -> State Env R.TagSet
transTagSet tagset = case tagset of
  All         -> return R.All
  NilT tag    -> return (R.List (R.Or [transTag tag]))
  OR ts _ ts' -> liftM2 R.Union (transTagSet ts) (transTagSet ts')
  Diff ts ts' -> liftM2 R.Diff (transTagSet ts) (transTagSet ts')
  Cart ts ts' -> liftM2 R.Diff (transTagSet ts) (transTagSet ts')
  Named nm    -> getSet tagsets (showSetName nm)




--------------------------------------------------------------------------------
-- Contextual tests

transCond :: Cond -> State Env R.Context
transCond cond = case cond of
  -- Single contexts, barriers and set negations
  CondPos pos tags    -> do let (rpos,subr) = transPosition pos
                            ts <- transTagSet tags 
                            return (R.Ctx rpos R.Yes (mapSubr subr ts))
  CondBarrier p t bt  -> do ctx <- transCond (CondPos p t)
                            btags <- transTagSet bt
                            return (addBar R.Barrier btags ctx)
  CondCBarrier p t bt -> do ctx <- transCond (CondPos p t)
                            btags <- transTagSet bt
                            return (addBar R.CBarrier btags ctx)
  CondNotPos pos tags -> not_ `fmap` transCond (CondPos pos tags)
  CondNotBar p t bt   -> not_ `fmap` transCond (CondBarrier p t bt)
  CondNotCBar p t bt  -> not_ `fmap` transCond (CondCBarrier p t bt)

  -- Complex contexts: negate, link and inline template 
  CondNegate cond     -> R.Negate `fmap` transCond cond
  CondLinked conds    -> link `fmap` mapM transCond conds
  CondTemplInl templs -> do let conds = map (\(Template x) -> x) templs
                            templ `fmap` mapM transCond conds

-- Finally, named template is just retrieved from the environment.
  CondTemplate name   -> getSet templates (showSetName name)

  where 
    link = R.Link . R.And
    templ = R.Template . R.Or

    not_ :: R.Context -> R.Context
    not_ ctx = ctx { R.polarity = R.Not }

    addBar :: (R.TagSet -> R.Scan) -> R.TagSet -> R.Context -> R.Context
    addBar f tags ctx = let pos = (R.position ctx) { R.scan = f tags }
                        in ctx { R.position = pos }

--------------------------------------------------------------------------------
-- Positions and subreadings.
-- Subreadings are awkward, I wanted them to be part of Tag in my data type.
-- because when I parse Apertium tagged text, it's easier to match.
-- Or maybe I didn't really think it through. Maybe change someday.


transPosition :: Position -> (R.Position, Maybe R.Subpos)
transPosition x = case x of
  Exactly num         -> (R.Pos R.Exactly R.NC (read' num), Nothing)
  AtLeastPre num      -> (R.Pos R.AtLeast R.NC (read' num), Nothing)
  AtLeastPost num     -> transPosition (AtLeastPre num)
  AtLPostCaut1 num    -> (R.Pos R.AtLeast R.C (read' num), Nothing)
  AtLPostCaut2 num    -> transPosition (AtLPostCaut1 num)
  Cautious position   -> let (pos,sub) = transPosition position
                         in (pos { R.careful = R.C }, sub)
  Subreading num num' -> let (pos,_) = transPosition (Exactly num)
                         in (pos, transSubrTarget (SubrTarget num'))
  SubreadingStar num  -> let (pos,_) = transPosition (Exactly num)
                         in (pos, Just R.Wherever)

transSubrTarget :: Subr -> Maybe R.Subpos
transSubrTarget subr = case subr of
  SubrEmpty      -> Nothing
  SubrTargetStar -> Just R.Wherever
  SubrTarget num -> Just $ let i = read' num in
                    if i<0 then R.FromStart i else R.FromEnd i

mapSubr :: Maybe R.Subpos -> R.TagSet -> R.TagSet
mapSubr Nothing  ts = ts
mapSubr (Just s) ts = R.Subreading s `fmap` ts

read' :: Signed -> Int
read' (Signed x) = read x


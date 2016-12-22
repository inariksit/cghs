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
import Text.Regex.PCRE

type Result = ([String], [[R.Rule]]) -- sections

parse :: Bool -> String -> Result 
parse test s = case pGrammar (CG.Par.myLexer s) of
  Bad err  -> error err
  Ok  tree -> let (rls,env) = runState (parseRules tree) emptyEnv
              in  ( map (show.snd) (tagsets env) ++ map (show.snd) (templates env)
                 , rls )

--------------------------------------------------------------------------------

parseRules :: Grammar -> State Env [[R.Rule]]
parseRules (Sections secs) = mapM parseSection secs

parseSection :: Section -> State Env [R.Rule]
parseSection (Defs defs) =
 do putSet (bosString, R.bosSet) --in case the grammar doesn't specify boundaries 
    putSet (eosString, R.eosSet)
    rights `fmap` mapM addDecl defs

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

getSet :: (Env -> [(String,a)]) -> String -> State Env (Maybe a)
getSet f nm = lookup nm `fmap` gets f


-- maybe someday I want to do something with the Strings; now I'm just ignoring them
addDecl :: Def -> State Env (Either String R.Rule)
addDecl (SetDef s)   = do transSetDecl s >>= putSet
                          return (Left $ CG.Print.printTree s)
addDecl (TemplDef t) = do transTemplDecl t >>= putTempl
                          return (Left $ CG.Print.printTree t)
addDecl (RuleDef r)  = do rl <- transRule r
                          putRule rl
                          return (Right rl)


--------------------------------------------------------------------------------
-- Rule

transRule :: Rule -> State Env R.Rule
transRule x = case x of
  RemoveIf nm sr tags _ cs 
    -> do trg <- transTagSet tags
          ctx <- mapM transCond cs
          let trgSubr = maybe trg (mapSubr trg) (transSubr sr)
          let name = transName nm          
          return (R.R R.REMOVE name trgSubr (R.And ctx))
  RemoveAlways nm sr tags
    -> always `fmap` transRule (RemoveIf nm sr tags MaybeIF_IF [])
  SelectIf nm sr tags x cs 
    -> select `fmap` transRule (RemoveIf nm sr tags x cs)
  SelectAlways nm sr tags  
    -> select `fmap` transRule (RemoveAlways nm sr tags)
  MatchLemma lem rl       
    -> do rule@(R.R _ _ trg _) <- transRule rl 
          let lemTag = R.Lem (showLem lem)
          return (newTrg rule (addTag lemTag trg))
  MatchWF wf rl       
    -> do rule@(R.R _ _ trg _) <- transRule rl 
          let wfTag = R.WF (showWF wf)
          return (newTrg rule (addTag wfTag trg))
  

  where
    always rl = rl { R.context = R.And [R.Always] }
    select rl = rl { R.rtype = R.SELECT}
    newTrg rl ts = rl { R.target = ts }

    addTag :: R.Tag -> R.TagSet -> R.TagSet
    addTag tag (R.List ts) = R.List (R.Or [R.And [tag]] `mappend` ts)
    addTag tag _           = error "addTag: TODO I should implement other set operations for targets"


    transName (MaybeName1 (Id nm)) = R.Name nm
    transName MaybeName2           = R.NoName



--------------------------------------------------------------------------------
-- Sets: BOS/EOS; simple names (Adj); syntactic tags (@OBJ); weird stuff (<;>)
{-
showSetName :: SetName -> String
showSetName setname =
  case setname of
    SetName (UIdent name) -> name
    SetMeta (UIdent name) -> "<" ++ name ++ ">"
    SetSynt (UIdent name) -> "@" ++ name
-}

showId :: Id -> String
showId (Id str) = str

showLem :: Lem -> String
showLem (Lem s) = (drop 1 . reverse . drop 1 . reverse) s

showWF :: WordForm -> String
showWF (WordForm s) = (drop 2 . reverse . drop 2 . reverse) s

bosString = ">>>"
eosString = "<<<"

transSetDecl :: SetDecl -> State Env (String, R.TagSet)
transSetDecl setdecl =
  case setdecl of 
    Set nm tagset -> (,) (show nm) `fmap` transTagSet tagset
    List nm tags  -> do let tagLists = map transTag tags :: [TagList]
                        let setName = showId nm
                        return (setName, R.List (R.Or tagLists))

    BList -> return (bosString, R.bosSet)
    EList -> return (eosString, R.eosSet)


-- Templates: single or a list with ORs
transTemplDecl :: TemplDecl -> State Env (String, R.Context)
transTemplDecl templ = case templ of
  SingleTempl nm cond  -> (,) (showId nm) `fmap` transCond cond
  ListTempl   nm conds -> (,) (showId nm) `fmap` transCond (CondTemplInl conds)

--------------------------------------------------------------------------------
-- Tags and tagsets

type TagList = R.AndList R.Tag

--toTag :: String -> R.Tag
--toTag s = case s of
--  ('"':'<':_) -> R.WF (strip 2 s)
--  ('"':    _) -> R.Lem (strip 1 s)
--  _           -> R.Tag s
--  where 
--    strip :: Int -> String -> String
--    strip n = drop n . reverse . drop n . reverse


showTag :: Tag -> String
showTag t = case t of
  BOS -> bosString
  EOS -> eosString
  AND tags -> "(" ++ unwords (map show tags) ++ ")"
  Tag (Id s) -> s
  Lemma l -> showLem l
  WordF w -> showWF w
  --TODO: case-insensitive lemma/wordform + regex
  LemmaCI foo   -> showTag (Lemma foo)
  WordFCI foo   -> showTag (WordF foo)
  Regex foo     -> show foo

transTag :: Tag -> TagList
transTag tag = case tag of
  BOS -> R.And [R.BOS]
  EOS -> R.And [R.EOS]
  AND tags -> R.And $ concatMap (R.getAndList.transTag) tags  
  t@(Tag name) -> R.And [R.Tag (showTag t)]
  l@(Lemma nm) -> R.And [R.Lem (showTag l)]
  w@(WordF nm) -> R.And [R.WF (showTag w)]
  --TODO: case-insensitive lemma + regex
  LemmaCI foo   -> transTag (Lemma foo) 
  WordFCI foo   -> transTag (WordF foo)
  Regex foo     -> R.And [R.Rgx (show foo)] 



transTagSet :: TagSet -> State Env R.TagSet
transTagSet tagset = case tagset of
  All         -> return R.All
--  NilT tag    -> return (R.List (R.Or [transTag tag]))
  OR ts _ ts' -> liftM2 R.Union (transTagSet ts) (transTagSet ts')
  Diff ts ts' -> liftM2 R.Diff (transTagSet ts) (transTagSet ts')
  Cart ts ts' -> liftM2 R.Diff (transTagSet ts) (transTagSet ts')
  -- A single tag could be just a single tag, or a reference to a tag list.
  -- No way to decide that by the shape of the identifier, hence trying both ways.
  Named tag   -> do let tagName = showTag tag
                    tags <- getSet tagsets tagName
                    return $ case tags of
                      Nothing -> R.List (R.Or [transTag tag])
                      Just ts -> ts




--------------------------------------------------------------------------------
-- Contextual tests

transCond :: Cond -> State Env R.Context
transCond cond = case cond of
  -- Single contexts, barriers and set negations
  CondPos pos tags    -> do ts <- transTagSet tags
                            let (rpos,subr) = transPosition pos
                            let tsSubr = maybe ts (mapSubr ts) subr
                            return (R.Ctx rpos R.Yes tsSubr)
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
  CondTemplate name   -> fromJust `fmap` getSet templates (showId name)

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
  AtLeastPost num     -> (R.Pos R.AtLeast R.NC (read' num), Nothing)
  AtLPostCaut1 num    -> (R.Pos R.AtLeast R.C (read' num), Nothing)
  AtLeastPre num      -> transPosition (AtLeastPost num)
  AtLPostCaut2 num    -> transPosition (AtLPostCaut1 num)
  Cautious position   -> let (pos,sub) = transPosition position
                         in (pos { R.careful = R.C }, sub)
  Subreading num num' -> transPosition (Exactly num) `subr` transSubr (SubrTarget num')
  SubreadingStar num  -> transPosition (Exactly num) `subr` transSubr SubrTargetStar
  where 
    subr (pos,_) sr = (pos,sr)

transSubr :: Subr -> Maybe R.Subpos
transSubr subr = case subr of
  SubrEmpty      -> Nothing
  SubrTargetStar -> Just R.Wherever
  SubrTarget num -> Just $ let i = read' num in
                    if i<0 then R.FromStart i else R.FromEnd i


mapSubr :: R.TagSet -> R.Subpos -> R.TagSet
mapSubr tags sr = R.Subreading sr `fmap` tags


read' :: Signed -> Int
read' (Signed x) = read x


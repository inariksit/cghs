module Parse ( parseRules 
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


parseRules :: String -> ([String],[[R.Rule]]) -- sections
parseRules s = case pGrammar (CG.Par.myLexer s) of
  Bad err  -> error err
  Ok  tree -> let (rules,_env) = runState (parseCGRules tree) emptyEnv
              in ( concatMap lefts rules     --map (show.snd) named env ++ map (show.snd) templates env
                 , map rights rules )

--------------------------------------------------------------------------------


data Env = Env { named :: [(String, R.TagSet)]
               , inline :: [R.TagSet] -- keep track of (unnamed) inline tag sets
               , templates :: [(String, R.Context)] }

emptyEnv = Env [] [] []

newSet :: (String,R.TagSet) -> State Env ()
newSet x = modify $ \env -> env { named = x : named env }

newTempl :: (String,R.Context) -> State Env ()
newTempl x = modify $ \env -> env { templates = x : templates env }

newInlineSet :: R.TagSet -> State Env ()
newInlineSet x = modify $ \env -> env { inline = x : inline env }

--------------------------------------------------------------------------------

parseCGRules :: Grammar -> State Env [[Either String R.Rule]]
parseCGRules (Sections secs) = mapM parseSection secs

parseSection :: Section -> State Env [Either String R.Rule]
parseSection (Defs defs) =
 do mapM_ updateEnv defs 
    --in case the grammar doesn't specify boundaries 
    newSet (bosString, R.bosSet)
    newSet (eosString, R.eosSet)
    env <- get
    parseAndModify env defs []
                           
 where
  parseAndModify :: Env -> [Def] -> [Either String R.Rule] -> State Env [Either String R.Rule]
  parseAndModify e [] acc       = do return acc
  parseAndModify e (d:defs) acc = do let (e', strOrRl) = parseRules' e d
                                     put e'
                                     parseAndModify e' defs (strOrRl:acc)
  
  updateEnv :: Def -> State Env ()
  updateEnv (RuleDef r)  = return ()
  updateEnv (SetDef s)   = newSet =<< transSetDecl s
  updateEnv (TemplDef t) = newTempl =<< transTemplDecl t


  parseRules' :: Env -> Def -> (Env, Either String R.Rule)
  parseRules' e (RuleDef  r) = let (r', e') = runState (transRule r) e
                               in (e', Right r')
  parseRules' e (SetDef   s) = (e, Left $ CG.Print.printTree s)
  parseRules' e (TemplDef t) = (e, Left $ CG.Print.printTree t)

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
                        return (setName, R.TagList (R.OrList tagLists))

    BList -> return (bosString, R.bosSet)
    EList -> return (eosString, R.eosSet)


-- Templates: single or a list with ORs
transTemplDecl :: TemplDecl -> State Env (String, R.Context)
transTemplDecl templ = case templ of
  SingleTempl nm cond  -> undefined -- (,) (showSetName nm) `fmap` transCond cond
  ListTempl   nm conds -> undefined -- (,) (showSetName nm) `fmap` transTempls conds
--   where 
--    fromTemplate (Template cond) = cond
--    transTempls = transCondSet . map fromTemplate

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
  BOS -> R.AndList [R.BOS]
  EOS -> R.AndList [R.EOS]
  Lemma (Str s) -> R.AndList [toTag s]
  Tag (Id str)  -> R.AndList [toTag str]
  AND tags      -> R.AndList $ concatMap (R.getAndList.transTag) tags

  --TODO: case-insensitive lemma + regex
  LemmaCI foo   -> transTag (Lemma foo) 
  Regex foo     -> transTag (Lemma foo)
  -- /TODO


transTagSet :: TagSet -> State Env R.TagSet
transTagSet tagset = case tagset of
  All          -> return R.All
  NilT tag     -> return (R.TagList (R.OrList [transTag tag]))
  OR ts1 _ ts2 -> liftM2 R.Union (transTagSet ts1) (transTagSet ts2)
  Diff ts1 ts2 -> liftM2 R.Diff (transTagSet ts1) (transTagSet ts2)
  Cart ts1 ts2 -> liftM2 R.Diff (transTagSet ts1) (transTagSet ts2)

  Named nm     -> do let name = showSetName nm
                     env <- gets named
                     let tags = fromMaybe 
                                  (error $ "Tagset " ++ name ++ " not defined!")
                                  (lookup name env) 
                     return tags


--------------------------------------------------------------------------------
-- Conditions



--------------------------------------------------------------------------------
-- Rule

transRule = undefined
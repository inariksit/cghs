module CGHS.Parse ( 
    parse
  , printGrammar
  ) where

import qualified CGHS.Containers as C
import qualified CGHS.Rule as R
import CGHS.Utils ( tagSet2Readings )
import CGHS.Compact

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

type Result = ([(String,R.TagSet)], [[R.Rule]]) -- sections

parse :: Bool -> String -> Result 
parse compact s = case pGrammar (CG.Par.myLexer s) of
  Bad err  -> error err
  Ok  tree -> let (rls,env) = runState (parseRules compact tree) emptyEnv
               in ( tagsets env 
                  , map rights rls )

printGrammar :: Bool -> String -> String
printGrammar compact s = case pGrammar (CG.Par.myLexer s) of
  Bad err  -> error err
  Ok  tree -> let defs = evalState (parseRules compact tree) emptyEnv
              in  "DELIMITERS = \"<$.>\"<PUNT_PUNT>\" \"<$?>\"<PUNT_GALD>\" \"<$!>\"<PUNT_ESKL>\" <<s>> <</s>>;"
                  ++ (intercalate "\nSECTION\n" $ map (unlines.map show') defs)

 where
  show' :: Either String R.Rule -> String
  show' (Left  s) = s
  show' (Right r) = show r

--------------------------------------------------------------------------------

parseRules :: Bool -> Grammar -> State Env [[Either String R.Rule]]
parseRules compact (Sections secs) = parseSection compact `mapM` secs

parseSection :: Bool -> Section -> State Env [Either String R.Rule]
parseSection compact (Defs defs) =
 do putSet (bosString, R.bosSet) --in case the grammar doesn't specify boundaries 
    putSet (eosString, R.eosSet)
    addDecl compact `mapM` defs

--------------------------------------------------------------------------------


data Env = Env { rules ::[R.Rule]
               , tagsets :: [(String, R.TagSet)]
--               , inlineTagsets :: [R.TagSet] -- keep track of inline tag sets
               , templates :: [(String, R.Context)] 
               }

emptyEnv = Env [] [] []

putRule :: R.Rule -> State Env ()
putRule x = modify $ \env -> env { rules = x : rules env }

putSet :: (String,R.TagSet) -> State Env ()
putSet x = modify $ \env -> env { tagsets = x : tagsets env }

putTempl :: (String,R.Context) -> State Env ()
putTempl x = modify $ \env -> env { templates = x : templates env }

getSet :: (Env -> [(String,a)]) -> String -> State Env (Maybe a)
getSet f nm = lookup nm `fmap` gets f


-- The strings are used for printGrammar
addDecl :: Bool -> Def -> State Env (Either String R.Rule)
addDecl compact def = case def of
   TemplDef t -> do transTemplDecl t >>= putTempl
                    return (Left $ CG.Print.printTree t)
   SetDef s   -> do (setOrList, origSetName,origSet) <- transSetDecl s 
                    if compact 
                      then do let compactSet = compactStrings False origSet
                              putSet (origSetName, compactSet)

                              return (Left $ "SET " ++ origSetName ++  -- For testing "anything goes" tagsets, don't keep this!
--                              return (Left $ setOrList ++ " " ++ origSetName ++ 
                                           " = " ++ C.showInline compactSet ++ " ;")
                      else do putSet (origSetName, origSet)
                              return (Left $ CG.Print.printTree s)
   RuleDef r  -> do rl <- transRule r
                    if compact 
                      then do 
                        let newRl = compactRule rl
                        rls <- gets rules
                        if newRl `elem` rls
                          then return (Left $ "# Repeated rule: " ++ show newRl)
                          else do putRule newRl
                                  return (Right newRl)
                      else do
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
          return (R.R R.REMOVE name trgSubr (C.And ctx))
  RemoveAlways nm sr tags
    -> always `fmap` transRule (RemoveIf nm sr tags MaybeIF_IF [])
  SelectIf nm sr tags x cs 
    -> select `fmap` transRule (RemoveIf nm sr tags x cs)
  SelectAlways nm sr tags  
    -> select `fmap` transRule (RemoveAlways nm sr tags)
  MapIf maptags _ tags x cs 
    -> do rule <- transRule (RemoveIf MaybeName2 SubrEmpty tags x cs)
          mts <- transTagSet maptags
          return (map_ rule mts)
  MapAlways maptags t tags 
    -> always `fmap` transRule (MapIf maptags t tags MaybeIF_IF [])
  MatchLemma lem rl       
    -> do rule@(R.R _ _ trg _) <- transRule rl 
          let lemTag = R.Lem lem
          return (newTrg rule (addTag lemTag trg))
  MatchWF wf rl       
    -> do rule@(R.R _ _ trg _) <- transRule rl 
          let wfTag = R.WF (showWF wf)
          return (newTrg rule (addTag wfTag trg))
  

  where
    always rl = rl { R.context = C.And [R.Always] }
    select rl = rl { R.oper = R.SELECT}
    iff rl = rl { R.oper = R.IFF }
    map_ rl ts = rl { R.oper = R.MAP ts }
    newTrg rl ts = rl { R.target = ts }

    addTag :: R.Tag -> R.TagSet -> R.TagSet
    addTag tag (C.Set nm ts) = C.Set nm (C.Or [C.And [tag]] `mappend` ts)
    addTag tag _             = error "addTag: TODO I should implement other set operations for targets"


    transName (MaybeName1 (ComplexId nm)) = R.Name nm
    transName MaybeName2                  = R.NoName



--------------------------------------------------------------------------------
-- Sets: BOS/EOS; simple names (Adj); syntactic tags (@OBJ); weird stuff (<;>)

showId :: Id -> String
showId (SetName (ComplexId str)) = str
showId (SetSynt (ComplexId str)) = '@':str
showId (SetUnif foo) = showId foo --TODO: return to these if I need them someday
showId (SetMeta foo) = showId (SetName foo)
showId (SetSem foo) = showId (SetName foo) 

showWF :: WordForm -> String
showWF (WordForm s) = (drop 2 . reverse . drop 2 . reverse) s

bosString = ">>>"
eosString = "<<<"

transSetDecl :: SetDecl -> State Env (String, String, R.TagSet)
transSetDecl setdecl =
  case setdecl of 
    Set nm tagset -> (,,) "SET" (showId nm) `fmap` transTagSet tagset
    List nm tags  -> do tagLists <- mapM transTagList tags
                        let setName = showId nm
                        return ("LIST", setName, C.Set (C.SetName setName) (C.Or tagLists))

    BList -> return ("LIST", bosString, R.bosSet)
    EList -> return ("LIST", eosString, R.eosSet)


-- Templates: single or a list with ORs
transTemplDecl :: TemplDecl -> State Env (String, R.Context)
transTemplDecl templ = case templ of
  SingleTempl nm cond  -> (,) (showId nm) `fmap` transCond cond
  ListTempl   nm conds -> (,) (showId nm) `fmap` transCond (CondTemplInl conds)

--------------------------------------------------------------------------------
-- Tags and tagsets

type TagList = C.AndList R.Tag

showTag :: Tag -> String
showTag t = case t of
  BOS       -> bosString
  EOS       -> eosString
  And tags  -> "(" ++ unwords (map show tags) ++ ")"
  Tag id_   -> showId id_
  Lemma l   -> l
  WordF w   -> showWF w
  LemmaCI l -> "\"" ++ l ++ "\"i"
  WordFCI w -> show w ++ "i"
  Regex r   -> "\"" ++ r ++ "\"r"
  RegexCI r   -> "\"" ++ r ++ "\"ri"
  RegexIC r   -> "\"" ++ r ++ "\"ir"

transTag :: Tag -> TagList
transTag tag = case tag of
  BOS -> C.And [R.BOS]
  EOS -> C.And [R.EOS]
  And tags -> C.And $ concatMap (C.getAndList . transTag) tags
  s@(Tag (SetSynt _)) -> C.And [R.Synt (showTag s)]
  t@(Tag name) -> C.And [R.Tag (showTag t)]
  l@(Lemma nm) -> C.And [R.Lem (showTag l)]
  w@(WordF nm) -> C.And [R.WF (showTag w)]
  l@(LemmaCI _) -> C.And [R.Rgx (showTag l)]
  w@(WordFCI _) -> C.And [R.Rgx (showTag w)]
  r@(Regex   _) -> C.And [R.Rgx (showTag r)] 
  r@(RegexCI _) -> C.And [R.Rgx (showTag r)] 
  r@(RegexIC _) -> C.And [R.Rgx (showTag r)] 

transTagList :: Tag -> State Env TagList
transTagList tag = case tag of
  And ts -> do let tagNames = map showTag ts
               maybeTagsets <- mapM (getSet tagsets) tagNames
               case catMaybes maybeTagsets of
                [] -> --trace ("transTagSet: All good " ++ show tag) $ 
                       return (transTag tag)
                xs -> do -- The name has been found as a tagset name.
                         -- We perform an additional check: if it is just a list name that maps to itself,
                         -- e.g. LIST Adj = Adj (maybe something else) ; then it's valid.
                         nameMatches <- sequence 
                                         [ do set <- getSet tagsets name :: State Env (Maybe R.TagSet)
                                              case set of
                                                Nothing -> return (name,True) -- Nothing found, ie. it's not a tag name after all (why did it get into here in the first place?)
                                                Just s  -> do let setdefString = C.showInline s
                                                              return $ if name `isInfixOf` setdefString
                                                                        then (name ++ " = " ++ setdefString,True)
                                                                        else (name ++ " = " ++ setdefString,False)

                                                     | name <- tagNames ]
                         let nameDoesntMatch = filter (not.snd) nameMatches
                         return $
                             if null nameDoesntMatch
                              then transTag tag
                              else trace ("\ntransTagSet: Using a set name in place of a single tag: " ++ CG.Print.printTree tag ++ "\n" ++ show (map fst nameDoesntMatch)) 
                                      $ transTag tag
  _        -> return (transTag tag)


transTagSet :: TagSet -> State Env R.TagSet
transTagSet tagset = case tagset of
  All -> return C.All
  Diff ts ts' -> liftM2 C.Diff (transTagSet ts) (transTagSet ts')
  Cart ts ts' -> liftM2 C.Cart (transTagSet ts) (transTagSet ts')
  Union ts _ ts' -> liftM2 C.Union (transTagSet ts) (transTagSet ts')

  -- A tagset consisting of a single tag could be just that, or a named tagset.
  -- No way to decide that by the shape of the identifier, hence trying both ways.
  Named t@(And tags) 
            -> do taglist <- transTagList t
                  return $ C.Set C.Inline (C.Or [taglist])

  Named tag -> do let tagName = showTag tag
                  tags <- getSet tagsets tagName
                  return $ case tags of
                             Just ts -> ts
                             Nothing -> C.Set C.Inline (C.Or [transTag tag])


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
    link = R.Link . C.And
    templ = R.Template . C.Or

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
mapSubr tags sr = fmap (R.Subreading sr) `fmap` tags


read' :: Signed -> Int
read' (Signed x) = read x


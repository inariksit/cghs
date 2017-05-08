-- Helper functions to manipulate Rules and their parts

module CGHS.Utils where

import CGHS.Containers
import CGHS.Rule

import Control.Monad ( liftM2 )

import Data.Foldable ( fold )
import Data.List ( partition, sortBy ) 
import Data.Map ( fromListWith, elems )
import Data.Maybe ( catMaybes )
import Data.Monoid ( mappend )


--------------------------------------------------------------------------------
-- Operations on rules

filterRule :: (Tag -> Bool) -> Rule -> Rule
filterRule p rl = rl { target  = transformSet (fmap (filterReading p)) (target rl)
                     , context = fmap (filterContext p) (context rl) }


filterContext :: (Tag -> Bool) -> Context -> Context
filterContext p ctx = case ctx of
  Ctx _ _ ts  -> ctx { tags = transformSet (fmap (filterReading p)) ts }
  Link cs     -> Link (filterContext p `fmap` cs)
  Template cs -> Template (filterContext p `fmap` cs)
  Negate c    -> Negate (filterContext p c)
  Always      -> Always
---------------------------
-- Roughly equivalent rules

newtype RuleTargetEQ = TrgEq { trgEq :: Rule } deriving ( Show )

instance Eq RuleTargetEQ where
  (TrgEq rl) == (TrgEq rl') = roughlySameTarget (target rl) (target rl')

instance Ord RuleTargetEQ where
  compare rl rl' | rl == rl' = EQ
                 | otherwise = target (trgEq rl) `compare` target (trgEq rl')

-- Group sets of rules based on their targets.
-- Target equality defined by roughlySameTarget: disregard lexical tags, 
-- if one is a subset of other, then they're the same.
groupRules :: [Rule] -> [[Rule]]
groupRules rls = elems $ fromListWith (++) 
                    [ (rl, [trgEq rl]) | rl <- map TrgEq rls ]

-- Sort rules by the length of context. 
sortByContext :: [Rule] -> [Rule]
sortByContext = sortBy compareByContext

compareByContext :: Rule -> Rule -> Ordering
compareByContext r r' = (lc r `compare` lc r') `mappend`
                        (lts r `compare` lts r')  `mappend`
                        (lsc r `compare` lsc r')

 where
  lc = length . context
  lts = length . concatMap tagsets . context
  lsc = length . show . context

tagsets :: Context -> [TagSet]
tagsets ctx = case ctx of
  Ctx _ _ _   -> [tags ctx]
  Template cs -> getOrList $ fmap tags cs
  Link cs     -> getAndList $ fmap tags cs
  Negate c    -> tagsets c
  Always      -> []

roughlySameTarget :: TagSet -> TagSet -> Bool
roughlySameTarget ts ts' = case (normaliseTagsetRel ts,normaliseTagsetRel ts') of
  (Set _ x, Set _ y) 
    -> let noLex  = fmap (fst . removeLexReading) x :: OrList Reading
           noLex' = fmap (fst . removeLexReading) y :: OrList Reading
       in includes noLex noLex' || includes noLex' noLex

  _ -> False




--------------------------------------------------------------------------------
-- Tag sets

-- Tagset operations may manipulate the underlying (OrList Reading)s:
-- ● remove elements
-- ● add elements
-- ● specify the underspecified readings (= add new things inside them)
-- Diffs will be treated in CGSAT, because we need absolute complement.
normaliseTagsetRel :: TagSet -> TagSet 
normaliseTagsetRel set = maybe set (Set Inline) (go set)
 where
  go :: TagSet -> Maybe (OrList Reading)
  go set = case set of
    All      -> Nothing -- If All is part of a complex TagSet, it normalises to All.
    Diff _ _ -> Nothing 
    -- Diff must not be normalised here: eventually we want an
    -- absolute complement! Tagsets inside Diff can be normalised.

    Set _ readings  -- No further normalisation
        -> Just readings
    Union ts ts' -- Add elements to ts
        -> liftM2 mappend (go ts) (go ts') 
    Inters ts ts' -- Remove elements from ts
        -> liftM2 intersRds (go ts) (go ts')
    Cart ts ts'  -- Combine elements of ts and ts'
        -> do normTs  <- go ts :: Maybe (OrList Reading)
              normTs' <- go ts' :: Maybe (OrList Reading)
              Just (fold `fmap` sequenceA [normTs, normTs'])


intersRds :: OrList Reading -> OrList Reading -> OrList Reading
-- Intended behaviour:
--   adv adV (ada "very") `intersRds` ada == (ada "very")
--   adv adV ada `intersRds` (ada "very") == (ada "very")
-- Because (ada) is an underspecified reading, which includes
-- the more specified reading (ada "very")
intersRds rds rds' = Or $ catMaybes [ rd `moreSpecified` rd' 
                                       | rd  <- getOrList rds
                                       , rd' <- getOrList rds' ]
 where -- rd is more specified than rd', if all tags in rd' are in rd
       -- e.g. rd = (ada "very"), rd' = (ada)
  moreSpecified :: (Eq a, Foldable t) => t a -> t a -> Maybe (t a)
  moreSpecified rd rd' | includes rd rd' = Just rd
                       | includes rd' rd = Just rd'
                       | otherwise = Nothing

includes :: (Eq a, Foldable t) => t a -> t a -> Bool
includes t1 t2 = all (`elem` t1) t2

-- Filter operations for readings

filterReading :: (Tag -> Bool) -> Reading -> Reading
filterReading p = fst . partitionReading p

partitionReading :: (Tag -> Bool) -> Reading -> (Reading, [Tag])
partitionReading p (And rd) = (And good, bad)
  where (good,bad) = partition p rd

-- Special filter for lexical forms
removeLexReading :: Reading -> (Reading, [Tag])
removeLexReading = partitionReading (not.isLex)

isLex :: Tag -> Bool
isLex (Lem _) = True
isLex (WF _)  = True
isLex (Rgx _) = True
isLex _       = False

--------------------------------------------------------------------------------
-- Contextual tests

scopes :: Context 
       -> OrList Int -- Due to templates and *, one context may permit different widths
scopes c = case c of
  Always      -> Or [1]
  Ctx ps pl t -> Or [pos ps] -- (-1, 5*, 2 BARRIER foo)
  c@(Link _)  -> let cs = normaliseLinkedCtx c
                     lastC = last $ getAndList cs
                 in Or [pos $ position lastC]
  Template cs -> fold $ fmap scopes cs 
  Negate ctx  -> scopes ctx


normaliseLinkedCtx :: Context -> AndList Context
normaliseLinkedCtx (Link cs) = And (frst:fixPos posFrst rest [])
 where
  (frst,rest) = (head $ getAndList cs, tail $ getAndList cs)
  posFrst = pos $ position frst

  fixPos base []               res = res
  fixPos base (Ctx ps pl t:cs) res = --trace (show c ++ " " ++ show res) $
    let newBase = base + pos ps
        newPos = ps { pos = newBase }
    in fixPos newBase cs (Ctx newPos pl t:res)  

normaliseLinkedCtx _ = error "not a linked contextual test"

--------------------------------------------------------------------------------
-- Positions 

-- | When applied to a sentence and target, change a Position
-- into a concrete list of indices, where the contextual test should hold.
-- Empty OrList = position is out of scope.
normalisePosition :: Position -> Int -> Int -> OrList Int
normalisePosition posn senlen origin = case scan posn of 
  Exactly -> Or [ absInd | absInd >= 1 && absInd <= senlen ]
  _       -> Or absInds
 where
  relInd = pos posn
  absInd = origin + relInd
  absInds = if relInd<0 then [1 .. origin+relInd]
                else [origin+relInd .. senlen]

----------------------------------------------------------------------------
-- Alternative read functions

parseReadingApeSubr :: String -> Reading
parseReadingApeSubr str = And $ maintags ++ concat subtags
 where
  (mainr:subrs) = split (=='+') str
  maintags = map readTag $ filter (not.null) $ split isValid mainr
  subrs_ns = map FromStart [1..] `zip` map (split isValid) subrs :: [(Subpos,[String])]
  subtags = map (\(n, strs) -> map (Subreading n . readTag) strs) subrs_ns
  isValid = (=='<') 


parseReadingApe :: String -> Reading
parseReadingApe = And . map readTag . filter (not.null) . split (=='<')

-- If you just want to get a quick and dirty list of readings from tag sets.
-- Suitable e.g. for extracting readings from the grammar: we don't care about
-- the relations, just want to know all different readings someone ever wrote.
tagSet2Readings :: TagSet -> [Reading]
tagSet2Readings ts = case normaliseTagsetRel ts of
  All        -> []
  Set _ rds  -> getOrList rds
  Diff rs ts -> tagSet2Readings rs ++ tagSet2Readings ts -- TODO


-- reading Apertium format, not same as in cghs/Parse
readTag :: String -> Tag
readTag ">>>" = BOS
readTag "<<<" = EOS
readTag []    = error "empty tag"
readTag str | head str == '@' = Synt (init str)
            | last str == '>' = Tag (init str)
            | last str == '$' = WF (init str)
            | otherwise       = Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))
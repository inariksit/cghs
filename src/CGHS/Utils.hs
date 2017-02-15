-- Helper functions to manipulate Rules and their parts

module CGHS.Utils where

import CGHS.Containers
import CGHS.Rule

import Control.Monad ( liftM2 )

import Data.Foldable ( fold )
import Data.List ( findIndices, intercalate, intersect, (\\), nub, partition, sortBy )
import Data.Map ( fromListWith, elems )
import Data.Maybe ( catMaybes )
import Data.Monoid ( mappend )


--------------------------------------------------------------------------------
-- Operations on rules


---------------------------
-- Roughly equivalent rules

newtype RuleTargetEQ = TrgEq { trgEq :: Rule } deriving ( Show )

instance Eq RuleTargetEQ where
  (TrgEq rl) == (TrgEq rl') = roughlySameTarget (target rl) (target rl')

instance Ord RuleTargetEQ where
  compare rl rl' | rl == rl' = EQ
                 | otherwise = (target $ trgEq rl) `compare` (target $ trgEq rl')

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
                        (lsc r `compare` lsc r')

 where
  lc = length . context
  lsc = length . show . context

roughlySameTarget :: TagSet -> TagSet -> Bool
roughlySameTarget ts ts' = case (normaliseTagsetRel ts,normaliseTagsetRel ts') of
  (Set x, Set y) 
    -> let noLex  = fmap (fst . removeLexReading) x :: OrList Reading
           noLex' = fmap (fst . removeLexReading) y :: OrList Reading
       in includes noLex noLex' || includes noLex' noLex

  _ -> False


----------------------
-- Slimming down rules

compactRule :: Rule -> Rule
compactRule rl = rl { target  = compactTagset (target rl)
                    , context = fmap compactContext (context rl) }


compactContext :: Context -> Context
compactContext ctx = case ctx of
  Ctx _ _ ts  -> ctx { tags = compactTagset ts }
  Link cs     -> Link (fmap compactContext cs)
  Template cs -> Template (fmap compactContext cs)
  Negate c    -> Negate (compactContext c)
  Always      -> Always

--------------------------------------------------------------------------------
-- Tag sets

-- Sometimes grammarians write tagsets in a repetitive way: (a c) OR (a d) OR (b c) OR (b d)
-- This function does some easy checks, and tries to make such lists nicer.
compactTagset :: TagSet -> TagSet
compactTagset ts = case ts of
  Set rds    -> let (nonLexRds,lexTags) = unzip $ getOrList $ fmap removeLexReading rds :: ([Reading], [[Tag]])
                 in if allSame nonLexRds && 
                        all (atLeast 1 . getAndList) nonLexRds &&
                         atLeast 2 lexTags
                       then let lexSet = Set (Or (map And lexTags))
                                morSet = Set (Or (nub nonLexRds))
                             in Cart lexSet morSet
                       else ts
  Union _ _ -> let norm = normaliseTagsetRel ts
                   compNorm = compactTagset norm 
                in if norm == compNorm then ts else compNorm

  _         -> ts -- Tagset is already using some fanciness

allSame :: Eq a => [a] -> Bool
allSame (x:y:xs) = x==y && allSame (y:xs)
allSame _        = True

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast k (x:xs) = atLeast (k-1) xs


findRepetition :: TagSet -> [(Reading,Int)] -- The elements that repeat
findRepetition (Set ts) = lexOccs ++ rdOccs
 where 
  (nonLexRds,lexTags) = unzip $ getOrList $ fmap removeLexReading ts :: ([Reading], [[Tag]])
  lexOccs = nub [ (And lexTag, occurrences lexTag lexTags)
                  | lexTag <- lexTags 
                  , not (null lexTag)]
  rdOccs = sortBy greedy $
            nub [ (rd, occurrences rd nonLexRds)
                  | rd <- nonLexRds 
                  , not (null rd)]

  --greedy rd rd' = let oc  = occurrences rd nonLexRds
  --                    oc' = occurrences rd' nonLexRds
  greedy (rd,oc) (rd',oc') = let rdlen  = length (getAndList rd)
                                 rdlen' =  length (getAndList rd')
                             in (oc `compare` oc') `mappend` 
                                (rdlen `compare` rdlen')

  occurrences x xs = length $ findIndices (`includes` x) xs

findRepetition ts@(Union _ _) = findRepetition (normaliseTagsetRel ts)
findRepetition _ = []
--[ 
--  | (nonLexRds,lexTags) <- map removeLexReading (getOrList rds) 
--  , atLeast 3 (filter )]

-- ("aitzi" ADB)
--  ("aitzin" IZE ARR BIZ- ABL) 
--  ("aitzin"  IZE ARR BIZ- GEL)
--  ("aitzin"  IZE ARR BIZ- INE)  
--  ("aitzin"  IZE ARR ALA)
--("aldamen" IZE ARR BIZ- ABL) 
--("aldamen" IZE ARR BIZ- GEL)
--("aldamen"  IZE ARR BIZ- INE)  

--   ("aitzi" ADB) 
--OR ( (("aitzin") OR ("aldamen"))
--   + (IZE ARR BIZ-) 
--   + ((ABL) OR (GEL) OR (INE))
--   )

-- Tagset operations may manipulate the underlying (OrList Reading)s:
-- ● remove elements
-- ● add elements
-- ● specify the underspecified readings (= add new things inside them)
-- Diffs will be treated in CGSAT, because we need absolute complement.
normaliseTagsetRel :: TagSet -> TagSet 
normaliseTagsetRel set = maybe set Set (go set)
 where
  go :: TagSet -> Maybe (OrList Reading)
  go set = case set of
    All      -> Nothing -- If All is part of a complex TagSet, it normalises to All.
    Diff _ _ -> Nothing 
    -- Diff must not be normalised here: eventually we want an
    -- absolute complement! Tagsets inside Diff can be normalised.

    Set readings  -- No further normalisation
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

removeLexReading :: Reading -> (Reading, [Tag])
removeLexReading (And rd) = (And nolx, lx)
 where (lx,nolx) = partition isLex rd

isLex :: Tag -> Bool
isLex (Lem _) = True
isLex (WF _)  = True
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
  Set rds    -> getOrList rds
  All        -> [] --TODO ??
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
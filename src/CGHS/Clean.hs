module CGHS.Clean (
    compactRule
  , compactTagset
  , findRepetition

  ) where

import CGHS.Containers
import CGHS.Rule
import CGHS.Utils

import Data.List ( findIndices, nub, sortBy )
import Data.Monoid ( mappend )

--------------------------------------------------------------------------------
-- Find repetitive tagsets, and try to compact them

-- | Checks if some definition occurs many times in the same tagset,
-- and outputs the results in order from most repetitive. 
findRepetition :: TagSet -> [(Reading,Int)] -- The elements that repeat
findRepetition (Set ts) = lexOccs ++ rdOccs
 where 
  (nonLexRds,lexTags) = unzip $ getOrList $ fmap removeLexReading ts :: ([Reading], [[Tag]])
  lexOccs = nub [ (And lexTag, occurrences lexTag lexTags)
                  | lexTag <- lexTags 
                  , not (null lexTag)]
  rdOccs = reverse $ sortBy greedy $
            nub [ (rd, occurrences rd nonLexRds)
                  | rd <- nonLexRds 
                  , not (null rd)]

  greedy (rd,oc) (rd',oc') = let rdlen  = length (getAndList rd)
                                 rdlen' =  length (getAndList rd')
                             in (oc `compare` oc') `mappend` 
                                (rdlen `compare` rdlen')

  occurrences x xs = length $ findIndices (`includes` x) xs

findRepetition ts@(Union _ _) = findRepetition (normaliseTagsetRel ts)
findRepetition _ = []


-- | Special case: if a tagset is of the form ("a" c) OR ("a" d) OR ("b" c) OR ("b" d),
-- we compact the tagset to ("a" OR "b") + (c OR d).
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


-- | Eliminating tagset repetition from rules
compactRule :: Rule -> Rule
compactRule rl = rl { target  = compactTagset (target rl)
                    , context = fmap compactContext (context rl) }

-- | Eliminating tagset repetition from contexts
compactContext :: Context -> Context
compactContext ctx = case ctx of
  Ctx _ _ ts  -> ctx { tags = compactTagset ts }
  Link cs     -> Link (fmap compactContext cs)
  Template cs -> Template (fmap compactContext cs)
  Negate c    -> Negate (compactContext c)
  Always      -> Always
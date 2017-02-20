module CGHS.Clean (
    compactRule
  , compactTagset
  , compactStrings
  , findRepetition
  ) where

import CGHS.Containers
import CGHS.Parse
import CGHS.Rule
import CGHS.Utils

import Data.Char ( toLower )
import Data.List ( findIndices, groupBy, isInfixOf, maximumBy, nub, sortBy, stripPrefix )
import Data.Monoid ( mappend )

--------------------------------------------------------------------------------
-- Find repetitive tagsets, and try to compact them


--compactGrammar :: 


-- | Checks if some definition occurs many times in the same tagset,
-- and outputs the results in order from most repetitive. 
findRepetition :: TagSet -> [(Reading,Int)] -- The elements that repeat
findRepetition (Set _ ts) = lexOccs ++ rdOccs
 where 
  (nonLexRds,lexTags) = unzip $ getOrList $ fmap removeLexReading ts :: ([Reading], [[Tag]])
  lexOccs = nub [ (And lexTag, occurrences lexTag lexTags)
                  | lexTag <- lexTags 
                  , not (null lexTag)]
  rdOccs = sortBy (flip greedy) $
            nub [ (rd, occurrences rd nonLexRds)
                  | rd <- nonLexRds 
                  , not (null rd)]

  greedy (rd,oc) (rd',oc') = let rdlen  = length (getAndList rd)
                                 rdlen' = length (getAndList rd')
                             in (oc `compare` oc') `mappend` 
                                (rdlen `compare` rdlen')

  occurrences x xs = length $ findIndices (`includes` x) xs

findRepetition ts@(Union _ _) = findRepetition (normaliseTagsetRel ts)
findRepetition _ = []


-- | Finds tagsets that are identical except for case and such, converts them into regex
compactStrings :: TagSet -> TagSet
compactStrings = transformSet nubOr . fmap regexReading
 where
  nubOr :: OrList Reading -> OrList Reading
  nubOr (Or [r]) = Or [r]
  nubOr (Or rs) = Or $
    let sgrs = [ maximumBy compLen x | x <- groupBy eqReading (sortBy ordReading rs) ]
     in if sgrs /= sortBy ordReading rs then sgrs else rs

  compLen x y = length (show x) `compare` length (show y)

  eqReading :: Reading -> Reading -> Bool
  eqReading (And tags) (And tags') = and $ zipWith eqButMai tags tags'

  ordReading :: Reading -> Reading -> Ordering
  ordReading x y | eqReading x y = EQ
                 | otherwise     = compare (show x) (show y)

  eqButMai :: Tag -> Tag -> Bool
  eqButMai (WF word) (Rgx wordMai) = 
    case stripPrefix ('"':'<':word) wordMai of
      Just "(>\\\"<..._MAI)?>\"ir" -> True
      Nothing -> False
  eqButMai x@(Rgx _) y@(WF _) = eqButMai y x
  eqButMai x y = x == y

  regexReading :: Reading -> Reading
  regexReading rd =
    let (nonLexRd,lexTags) = removeLexReading rd
     in And (map regex lexTags) `mappend` nonLexRd 
  
regex :: Tag -> Tag
regex tag = case tag of
  WF x  -> if "_MAI" `isInfixOf` x
            then let wfStart = takeWhile (/='>') x
                 in Rgx ( "\"<" ++ map toLower wfStart ++ "(>\\\"<..._MAI)?>\"ir" )
            else tag
  _     -> tag


-- | Special case: if a tagset is of the form ("a" c) OR ("a" d) OR ("b" c) OR ("b" d),
-- we compact the tagset to ("a" OR "b") + (c OR d).
compactTagset :: TagSet -> TagSet
compactTagset ts = 
  let reTs = compactStrings ts
  in case reTs of
      Set nm rds -> let (nonLexRds,lexTags) = unzip $ getOrList $ fmap removeLexReading rds :: ([Reading], [[Tag]])
                    in if allSame nonLexRds && 
                           all (atLeast 2 . getAndList) nonLexRds &&
                           atLeast 2 lexTags
                        then let lexSet = Set Inline (Or (map And lexTags))
                                 morSet = Set Inline (Or (nub nonLexRds))
                              in Cart lexSet morSet
                        else reTs
      Union _ _ -> let norm = normaliseTagsetRel reTs
                       compNorm = compactTagset norm 
                    in if norm == compNorm then reTs else compNorm

      _         -> reTs -- Tagset is already using some fanciness
  
allSame :: Eq a => [a] -> Bool
allSame (x:y:xs) = x==y && allSame (y:xs)
allSame _        = True

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast k (x:xs) = atLeast (k-1) xs

-------------------------------------------------------------------------------

-- | Eliminating tagset repetition from rules
compactRule :: Rule -> Rule
compactRule rl = rl { target  = compactStrings (compactTagset (target rl))
                    , context = fmap compactContext (context rl) }

-- | Eliminating tagset repetition from contexts
compactContext :: Context -> Context
compactContext ctx = case ctx of
  Ctx _ _ ts  -> ctx { tags = compactStrings (compactTagset ts) }
  Link cs     -> Link (fmap compactContext cs)
  Template cs -> Template (fmap compactContext cs)
  Negate c    -> Negate (compactContext c)
  Always      -> Always
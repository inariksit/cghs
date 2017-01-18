-- Helper functions to manipulate Rules and their parts

module Utils ( scopes 
             , normaliseLinkedCtx 
             , normalisePosition 
             , normaliseTagsetRel 
             )
where

import Rule

import Control.Monad ( liftM2 )

import Data.Foldable ( fold )
import Data.List ( intercalate, intersect, (\\) )
import Data.Maybe ( catMaybes )


--------------------------------------------------------------------------------
-- Tag sets

-- Tagset operations may manipulate the underlying (OrList Reading)s:
-- ● remove elements
-- ● add elements
-- ● specify the underspecified readings (= add new things inside them)
-- We can'no't treat Diffs properly, because we need absolute complement.
-- Maybe this doesn't make any sense here, and I should only have a
-- normaliseTagsetAbs in CG_SAT?
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
  moreSpecified rd rd' | all (`elem` rd) rd' = Just rd
                       | all (`elem` rd') rd = Just rd'
                       | otherwise = Nothing

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
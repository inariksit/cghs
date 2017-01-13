{-# LANGUAGE FlexibleInstances #-}

module Main where

import Rule
import Parse
import Control.Monad ( liftM3, liftM4 )

import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do verboseCheck checkRule
          putStrLn "--------------"
          putStrLn "Now unit tests:"
          runTestTT $ TestList [ testNormaliseRelInters1
                               , testNormaliseRelInters2
                               , testNormaliseRelCart 
                               , testNormaliseRelUnion
                               , testNormaliseLinkedCtx1 
                               , testNormaliseLinkedCtx2 
                               , testNormaliseLinkedCtx3 
                               , testNormaliseLinkedCtx4 
                               ]
          putStrLn ""

--------------------------------------------------------------------------------
--just for fun, to see automatically generated CG rules
checkRule :: Rule -> Bool
checkRule rule = True


--------------------------------------------------------------------------------
-- Some trivial unit tests

testNormaliseLinkedCtx1 :: Test
testNormaliseLinkedCtx1 = 
  TestCase $ assertEqual "normaliseLinkedCtx" 
             normC1 (normaliseLinkedCtx (head $ getAndList ctx1))

testNormaliseLinkedCtx2 :: Test
testNormaliseLinkedCtx2 = 
  TestCase $ assertEqual "normaliseLinkedCtx" 
             normC2 (normaliseLinkedCtx (head $ getAndList ctx2))

testNormaliseLinkedCtx3 :: Test
testNormaliseLinkedCtx3 = 
  TestCase $ assertEqual "normaliseLinkedCtx" 
            normC3 (normaliseLinkedCtx (head $ getAndList ctx3))

testNormaliseLinkedCtx4 :: Test
testNormaliseLinkedCtx4 = 
  TestCase $ assertEqual "normaliseLinkedCtx" 
            normC4 (normaliseLinkedCtx (head $ getAndList ctx4))



(_,linkRules) = parse "SECTION \
\ SELECT (a) IF (1  (b) LINK 0 (c) LINK 1  (d)) ; \
\ SELECT (a) IF (1 (b))     (1 (c))     (2 (d)) ; \

\ SELECT (a) IF (1  (b) LINK 0 (c) LINK -1 (d)) ; \
\ SELECT (a) IF (1 (b))     (1 (c))     (0 (d)) ; \

\ SELECT (a) IF (-1 (b) LINK 0 (c) LINK 1  (d)) ; \
\ SELECT (a) IF (-1 (b))   (-1 (c))     (0 (d)) ; \

\ SELECT (a) IF (-1 (b) LINK 0 (c) LINK -1 (d)) ; \
\ SELECT (a) IF (-1 (b))   (-1 (c))    (-2 (d)) ;"
(ctx1:normC1:ctx2:normC2:ctx3:normC3:ctx4:normC4:_) = map context (concat linkRules)

testNormaliseRelInters1 :: Test
testNormaliseRelInters1 = TestCase $ assertEqual "(ada) ∩ (ada \"very\") results in (ada \"very\")" 
                                     (normaliseRel (Inters ada adaVery)) (adaVery) 

testNormaliseRelInters2 :: Test
testNormaliseRelInters2 = TestCase $ assertEqual "(ada \"very\")  ∩ (ada) results in (ada \"very\")" 
                                     (normaliseRel (Inters adaVery ada)) (adaVery) 


ada :: TagSet
ada = Set (Or [And [Tag "ada"]])

adaVery :: TagSet
adaVery = Set (Or [And [Tag "ada", Lem "very"]])


testNormaliseRelUnion :: Test
testNormaliseRelUnion = TestCase $ assertEqual "Union foo bar == foo bar"
                                   (normaliseRel (Union adjN gend))
                                   (Set (Or [adj,n,f,m]))

testNormaliseRelCart :: Test
testNormaliseRelCart = TestCase $ assertEqual "adj OR n + m OR f OR mf == (adj m),(adj f),(n m),(n f)"
                                   (normaliseRel (Cart gend adjN)) 
                                   (Set (Or [ And [Tag "f", Tag "adj"]
                                            , And [Tag "f", Tag "n"]
                                            , And [Tag "m", Tag "adj"]
                                            , And [Tag "m", Tag "n"]
                                   ]))
n :: Reading
n = And [Tag "n"]

adj :: Reading
adj = And [Tag "adj"]

adjN :: TagSet
adjN = Set (Or [adj, n])

m = And [Tag "m"]
f = And [Tag "f"]

gend :: TagSet
gend = Set (Or [f, m])

--------------------------------------------------------------------------------

instance Arbitrary Rule where
  arbitrary = liftM4 R arbitrary arbitrary arbitrary arbitrary

instance Arbitrary Oper where
  arbitrary = frequency [ (50, return REMOVE) 
                        , (45, return SELECT)
                        , (30, fmap MAP arbitrary)
                        , (10, return IFF)
                        , (10, return ADD)
                        , (10, return SUBSTITUTE)
                        ]
instance Arbitrary Name where
  arbitrary = frequency [ (10, return NoName) 
                        , (1, fmap Name (arbitrary `suchThat` atMost 10)) ]
--------------------------------------------------------------------------------

morfStrings :: [String]
morfStrings = ["vblex", "vbser", "vbmod", "n", "np", "det",
              "adv", "cnjcoo", "cnjsub", "prep", "sg", "pl"] -- etc.

syntStrings :: [String]
syntStrings = ["@SUBJ", "@OBJ", "@+JADNAG", "@-JADLAG"]

wordStrings :: [String]
wordStrings = ["buruz","atzera","behera","aurrera","hur","inguru",
               "kanpo","ondo","oste","pare","aitzin","erdiko","eskeko"]

instance Arbitrary Tag where
  arbitrary = elements (  map Tag morfStrings
                       ++ map Synt syntStrings
                       ++ map Lem wordStrings
                       ++ map WF wordStrings
                       ++ [EOS,BOS] ) 

--------------------------------------------------------------------------------

instance Arbitrary Position where
  arbitrary = liftM3 Pos arbitrary arbitrary arbitrary

instance Arbitrary Careful where
  arbitrary = elements [C,NC]

instance Arbitrary Scan where
  arbitrary = do btags <- arbitrary
                 elements [ Exactly, AtLeast, Barrier btags, CBarrier btags ]

--------------------------------------------------------------------------------

instance Arbitrary Context where
  arbitrary = do posn <- arbitrary
                 polr <- arbitrary
                 tgst <- arbitrary
                 let simpleContext = Ctx posn polr tgst
                 frequency [ (70, return simpleContext)
                           , (20, return $ Negate simpleContext)
                           , (10, return Always)
                           , (1, fmap Link arbitrary)
                           , (1, fmap Template arbitrary)
                           ]

instance Arbitrary Polarity where
  arbitrary = elements [ Yes, Not ]

--------------------------------------------------------------------------------

instance Arbitrary (Set OrList Reading) where
  arbitrary = do as <- Set `fmap` arbitrary
                 bs <- Set `fmap` arbitrary
                 frequency [ (3, return All)
                           , (30, return as)
                           , (1,  return $ Union as bs)
                           , (10, return $ Diff as bs)
                           , (10, return $ Cart as bs)
                           ]

instance (Arbitrary a) => Arbitrary (AndList a) where
  arbitrary = fmap And (listOf arbitrary `suchThat` atMost 5)

instance (Arbitrary a) => Arbitrary (OrList a) where
  arbitrary = fmap Or (listOf arbitrary `suchThat` atMost 5)

atMost :: Int -> [a] -> Bool
atMost n as = length as <= n && not (null as)
--------------------------------------------------------------------------------
module Main where

import Rule
import Parse
import Control.Monad ( liftM3, liftM4 )
import Test.QuickCheck

main :: IO ()
main = verboseCheck checkRule

--just for fun, to see automatically generated CG rules
checkRule :: Rule -> Bool
checkRule rule = True

--------------------------------------------------------------------------------

instance Arbitrary Rule where
  arbitrary = liftM4 R arbitrary arbitrary arbitrary arbitrary

instance Arbitrary RType where
  arbitrary = frequency [ (50, return REMOVE) 
                        , (45, return SELECT)
                        , (30, fmap MAP arbitrary)
                        , (10, return IFF)
                        , (10, return ADD)
                        , (10, return SUBSTITUTE)
                        ]
instance Arbitrary RName where
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
                 frequency $ [ (70, return simpleContext)
                             , (20, return $ Negate simpleContext)
                             , (10, return Always)
                             , (1, fmap Link arbitrary)
                             , (1, fmap Template arbitrary)
                             ]

instance Arbitrary Polarity where
  arbitrary = elements [ Yes, Not ]

--------------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do as <- listOf arbitrary `suchThat` atMost 5
                 bs <- listOf arbitrary `suchThat` atMost 5
                 let simpleAs  = List (Or [And as])
                 let simpleBs  = List (Or [And bs])
                 let simpleABs = List (Or [And as, And bs])
                 frequency [ (3, return All)
                           , (30, return simpleAs)
                           , (50, return simpleABs)
                           , (1,  return $ Union simpleAs simpleBs)
                           , (10, return $ Diff simpleAs simpleBs)
                           , (10, return $ Cart simpleAs simpleBs)
                           ]

instance (Arbitrary a) => Arbitrary (AndList a) where
  arbitrary = fmap And (listOf arbitrary `suchThat` atMost 5)

instance (Arbitrary a) => Arbitrary (OrList a) where
  arbitrary = fmap Or (listOf arbitrary `suchThat` atMost 5)

atMost :: Int -> [a] -> Bool
atMost n as = length as <= n && not (null as)
--------------------------------------------------------------------------------
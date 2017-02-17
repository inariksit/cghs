{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}

module CGHS.Containers where 

import Data.List ( intercalate )

--------------------------------------------------------------------------------
-- General-purpose helper data types

-- Newtypes for lists. 
-- More verbose, but hopefully less prone to errors, and less ad hoc show functions.

-- In the rules and tagsets, we need both conjunctions and disjunctions, such as
-- Conjunctions: 
-- ● Tags enclosed in parentheses: ("warm" adj), (vblex sg p3)
-- ● Linked contextual tests: (-1 Adj LINK 1 Noun)
-- ● List of contextual tests in a rule: REMOVE Adj IF (NOT -1 Det) (NOT 1 Noun)

-- Disjunctions:
-- ● Tags not in parentheses: vblex vbser vbaux
-- ● Tag sets in union: Noun | (PropNoun - Toponym)
-- ● Contexts inside a template: ((-1C Det) OR (NOT 1 Noun))
newtype OrList a = Or { getOrList :: [a] } 
  deriving (Eq,Ord,Functor,Foldable,Monoid,Applicative,Traversable)

newtype AndList a = And { getAndList :: [a] } 
 deriving (Eq,Ord,Functor,Foldable,Monoid,Applicative,Traversable)

instance (Show a) => Show (AndList a) where
  show = unwords . map show . getAndList


--------------------------------------------------------------------------------
-- Specific structure for VISL CG-3 tag sets and lists.
-- It's polymorphic just so that I can use fmap and stuff.

data Set t a = Set (t a)                  -- LIST Foo = foo ("<bar>" bar) baz
            | Union (Set t a) (Set t a)       -- SET Baz = Foo | Bar
            | Diff (Set t a) (Set t a)        -- SET Baz = Foo - Bar
            | Cart (Set t a) (Set t a)        -- SET Baz = Foo + Bar
--            | SymDif (Set a) (Set a)        -- SET Baz = Foo ∆ Bar
            | Inters (Set t a) (Set t a)      -- SET Baz = Foo ∩ Bar
            | All deriving (Eq,Ord)           -- (*)

instance (Functor t) => Functor (Set t) where
  fmap f (Set ts)       = Set (fmap f ts)
  fmap f (Union ts ts') = Union (fmap f ts) (fmap f ts')
  fmap f (Diff ts ts')  = Diff (fmap f ts) (fmap f ts')
  fmap f (Cart ts ts')  = Cart (fmap f ts) (fmap f ts')
  fmap f All            = All

instance (Show (t a)) => Show (Set t a) where
  show (Set ts) = show ts
  show (Union ts ts') = show ts ++ " | " ++ show ts'
  show (Inters ts ts') = show ts ++ " ∩ " ++ show ts'
  show (Diff ts ts') = show ts ++ " - " ++ show ts'
  show (Cart ts ts') = show ts ++ " + " ++ show ts'
  show All = "(*)"

transformSet :: (Eq a) => (t a -> t a) -> Set t a -> Set t a
transformSet f (Set x)      = Set (f x)
transformSet f (Union x y)  = Union (transformSet f x) (transformSet f y)
transformSet f (Inters x y) = Inters (transformSet f x) (transformSet f y)
transformSet f (Diff x y)   = Diff (transformSet f x) (transformSet f y)
transformSet f (Cart x y)   = Cart (transformSet f x) (transformSet f y)
transformSet f All          = All
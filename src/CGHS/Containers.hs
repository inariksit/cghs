{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}

module CGHS.Containers where 

import Data.List ( intercalate )
import Data.Foldable ( fold )

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

data SetName = SetName String | Inline
  deriving (Show,Eq,Ord)

data Set t a = Set SetName (t a)              -- LIST Foo = foo ("<bar>" bar) baz
            | Union (Set t a) (Set t a)       -- SET Baz = Foo | Bar
            | Diff (Set t a) (Set t a)        -- SET Baz = Foo - Bar
            | Cart (Set t a) (Set t a)        -- SET Baz = Foo + Bar
--            | SymDif (Set a) (Set a)        -- SET Baz = Foo ∆ Bar
            | Inters (Set t a) (Set t a)      -- SET Baz = Foo ∩ Bar
            | All deriving (Eq,Ord)           -- (*)

instance (Functor t) => Functor (Set t) where
  fmap f (Set nm ts)    = Set nm (fmap f ts)
  fmap f (Union ts ts') = Union (fmap f ts) (fmap f ts')
  fmap f (Diff ts ts')  = Diff (fmap f ts) (fmap f ts')
  fmap f (Cart ts ts')  = Cart (fmap f ts) (fmap f ts')
  fmap f All            = All

instance (Show (t a)) => Show (Set t a) where
--  show = showInline
  show (Set  Inline    ts) = show ts
  show (Set (SetName s) _) = s
--  show (Set (SetName s) ts) = s ++ "[" ++ show ts ++ "]" 
  show (Union ts ts') = show ts ++ " | " ++ show ts'
  show (Inters ts ts') = show ts ++ " ∩ " ++ show ts'
  show (Diff ts ts') = show ts ++ " - " ++ show ts'
  show (Cart ts ts') = show ts ++ " + " ++ show ts'
  show All = "(*)"

-- | Show function that forces the full set, even when name is given
showInline :: (Show (t a)) => Set t a -> String
showInline ts = case ts of
  Set _ tags -> show (Set Inline tags)
  Union t t' -> "(" ++ showInline t ++ ") | (" ++ showInline t' ++ ")"
  Inters t t' -> showInline t ++ " ∩ " ++ showInline t'
  Diff t t' -> "(" ++ showInline t ++ ") - (" ++ showInline t' ++ ")"
  Cart t t' -> "(" ++ showInline t ++ ") + (" ++ showInline t' ++ ")"
  All -> "(*)"

--------------------------------------------------------------------------------
-- Other operations

transformSet :: (Eq a) => (t a -> t a) -> Set t a -> Set t a
transformSet f ts = case ts of 
  Set nm x   -> Set nm (f x)
  Union x y  -> Union (transformSet f x) (transformSet f y)
  Inters x y -> Inters (transformSet f x) (transformSet f y)
  Diff x y   -> Diff (transformSet f x) (transformSet f y)
  Cart x y   -> Cart (transformSet f x) (transformSet f y)
  All        -> All

-- Loosen the requirements: any tag mentioned in the tagset will do
anyTag :: OrList (AndList a) -> OrList (AndList a)
anyTag (Or ts) = Or $ getAndList $ (\t -> And [t]) `fmap` fold ts
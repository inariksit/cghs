module Rule where

import Data.List ( intercalate )
import Text.Printf ( printf )
import Text.Regex.PCRE


--------------------------------------------------------------------------------
-- Rules

data Rule = 
    R { rtype :: RType 
      , target :: TagSet
      , context :: AndList Context
      } deriving (Eq,Ord,Show)

data RType = SELECT | REMOVE | IFF | ADD | MAP | SUBSTITUTE
   deriving (Eq,Show,Ord)

--------------------------------------------------------------------------------
-- Tags and tagsets

type TagSet = Set Tag

-- Specific structure for VISL CG-3 tag sets and lists.
-- It's polymorphic just so that I can use fmap and stuff.
data Set a = List (OrList (AndList a))    -- LIST Foo = foo ("<bar>" bar) baz
            | Union (Set a) (Set a)       -- SET Baz = Foo | Bar
            | Diff (Set a) (Set a)        -- SET Baz = Foo - Bar
            | Cart (Set a) (Set a)        -- SET Baz = Foo + Bar
--            | SymDif (Set a) (Set a)      -- SET Baz = Foo ∆ Bar
--            | Inters (Set a) (Set a)      -- SET Baz = Foo ∩ Bar
            | All deriving (Eq,Ord,Show)  -- (*)

tagList :: Tag -> TagSet
tagList tag = List (Or [(And [tag])])

bosSet :: TagSet
bosSet = List (Or [And [BOS]])

eosSet :: TagSet
eosSet = List (Or [And [EOS]])

data Tag = Tag String 
         | Lem String 
         | WF String 
         | Subreading Subpos Tag 
         | Rgx String 
         | EOS | BOS deriving (Eq,Ord)

data Subpos = FromStart Int | FromEnd Int | Wherever

-- | Following the conventions of vislcg3
instance Show Tag where
  show (WF str)  = printf "\"<%s>\"" str
  show (Lem str) = printf "\"%s\"" str
  show (Rgx str) =  printf "\"%s\"r" str
  show (Tag str) = str
  show (Subreading n tag)
                   = printf "%s+%s" (show n) (show tag)
  show BOS       = ">>>"
  show EOS       = "<<<"


instance Show Subpos where
  show (FromStart n) = show n
  show (FromEnd   n) = show (-n)
  show Wherever = "*"

-- TODO: we should know how many levels there are in total,
-- in order to compare FromStart and FromEnd.
-- Let's hope nobody writes grammars full of + and - subreadings.
instance Eq Subpos where
  Wherever    == anywhere    = True
  anywhere    == Wherever    = True
  FromStart n == FromStart m = n==m
  FromEnd   n == FromEnd   m = n==m
  _           == _           = False

instance Ord Subpos where
  Wherever    `compare` anywhere    = EQ
  anywhere    `compare` Wherever    = EQ
  FromStart n `compare` FromEnd   m = GT
  FromEnd   n `compare` FromStart m = LT
  FromStart n `compare` FromStart m = n `compare` m
  FromEnd   n `compare` FromEnd   m = n `compare` m



--------------------------------------------------------------------------------
-- Contextual tests

-- There is a lot of list action happening here, instead of in Rule.
-- Why is that? It's because of NEGATE: it's meaningful to negate a
-- linked context, or a template (named at least, maybe also inline?).
-- There's still a top level AndList Context in the Rule data type:
-- those can be anything specified here.
-- REMOVE Foo IF (-1 Bar)  -- regular Ctx
--               (-2 Baz LINK 1 Quux LINK T:Xyzzy)  -- Link
--               (NEGATE T:tmpl) -- Negate + Template
--               ( (-1 Foo) OR (1 Bar) ); -- Inline template
-- This rule has 3 contexts, and all of them must hold
data Context = Ctx { position :: Position 
                   , polarity :: Polarity
                   , tags :: Set Tag 
                   } 
             | Link (AndList Context)
             | Template (OrList Context) -- same for inline and named
             | Negate Context
             | Always deriving (Eq,Ord,Show)

data Position = Pos { scan :: Scan 
                    , careful :: Careful
                    , pos :: Int 
                    } deriving (Eq,Ord,Show)

data Scan = Exactly 
          | AtLeast 
          | Barrier TagSet 
          | CBarrier TagSet deriving (Eq,Ord,Show)
data Careful  = C | NC deriving (Eq,Ord,Show)
data Polarity = Posi | Nega deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- general-purpose stuff

-- I was just tired of using [[Tag]] and remembering if the outer is disjunction and inner conjunction or vice versa.
-- More verbose but maybe less errors.
newtype OrList a = Or { getOrList :: [a] } deriving (Eq,Ord)
newtype AndList a = And { getAndList :: [a] } deriving (Eq,Ord)


instance (Show a) => Show (AndList a) where
  show = filter (/='"') . unwords . map show . getAndList
instance Functor AndList where
  fmap f (And xs) = And (fmap f xs)
instance Foldable AndList where
  foldMap f (And xs) = foldMap f xs

instance (Show a) => Show (OrList a) where
  show = filter (/='"') . intercalate " OR " . map show . getOrList
instance Functor OrList where
  fmap f (Or xs) = Or (fmap f xs)
instance Foldable OrList where
  foldMap f (Or xs) = foldMap f xs


instance Functor Set where
  fmap f (List ts)      = List (fmap (fmap f) ts)
  fmap f (Union ts ts') = Union (fmap f ts) (fmap f ts')
  fmap f (Diff ts ts')  = Diff (fmap f ts) (fmap f ts')
  fmap f (Cart ts ts')  = Cart (fmap f ts) (fmap f ts')
  fmap f All            = All



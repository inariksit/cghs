module Rule where

import Data.List ( intercalate )
import Text.Printf ( printf )
import Text.Regex.PCRE


--------------------------------------------------------------------------------
-- Rules

data Rule = 
    R { rtype :: RType 
      , target :: TagSet
      , context :: OrList (AndList Context) 
      } deriving (Eq,Ord,Show)

data RType = SELECT | REMOVE | IFF | ADD | MAP | SUBSTITUTE
   deriving (Eq,Show,Ord)

--------------------------------------------------------------------------------
-- Tags and tagsets


data TagSet = TagList (OrList (AndList Tag)) -- LIST Foo = foo ("<bar>" bar) baz
            | Union TagSet TagSet            -- SET Baz = Foo | Bar
            | Diff TagSet TagSet             -- SET Baz = Foo - Bar
            | Cart TagSet TagSet             -- SET Baz = Foo + Bar
--            | SymDif TagSet TagSet             -- SET Baz = Foo ∆ Bar
--            | Inters TagSet TagSet             -- SET Baz = Foo ∩ Bar
            | All deriving (Eq,Ord,Show)

tagList :: Tag -> TagSet
tagList tag = TagList (OrList [(AndList [tag])])

bosSet :: TagSet
bosSet = TagList (OrList [AndList [BOS]])

eosSet :: TagSet
eosSet = TagList (OrList [AndList [EOS]])

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

data Context = Ctx { position :: Position 
                   , polarity :: Polarity
                   , tags :: TagSet 
                   } deriving (Eq,Ord,Show)

data Position = Pos { scan :: Scan 
                    , careful :: Careful
                    , pos :: Int 
                    } deriving (Eq,Ord,Show)

data Scan     = Exactly | AtLeast | Barrier | CBarrier deriving (Eq,Ord,Show)
data Careful  = C | NC deriving (Eq,Ord,Show)
data Polarity = Posi | Nega deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- general-purpose stuff

-- I was just tired of using [[Tag]] and remembering if the outer is disjunction and inner conjunction or vice versa.
-- More verbose but maybe less errors.
newtype OrList a = OrList { getOrList :: [a] } deriving (Eq,Ord)
newtype AndList a = AndList { getAndList :: [a] } deriving (Eq,Ord)


instance (Show a) => Show (AndList a) where
  show = filter (/='"') . unwords . map show . getAndList

instance (Show a) => Show (OrList a) where
  show = filter (/='"') . intercalate " OR " . map show . getOrList

-- to be able to use concat, concatMap etc. directly. (Hope this works :-P)
instance Foldable (AndList) where
  foldMap f (AndList xs) = foldMap f xs

instance Foldable (OrList) where
  foldMap f (OrList xs) = foldMap f xs


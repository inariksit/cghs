{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Rule where

import Data.List ( intercalate )
import Text.Printf ( printf )
import Text.Regex.PCRE


--------------------------------------------------------------------------------
-- Rules

data Rule = 
    R { rtype :: RType 
      , rname :: RName
      , target :: TagSet
      , context :: AndList Context
      } deriving (Eq,Ord)

data RType = SELECT | REMOVE | IFF | ADD | MAP TagSet | SUBSTITUTE
  deriving (Eq,Ord,Show)

data RName = Name String | NoName
  deriving (Eq,Ord)

instance Show RName where
  show NoName   = []
  show (Name s) = ':':s

instance Show Rule where
  show (R t n trg (And [Always])) = printf "%s%s %s ;"  (show t) (show n) (show trg)
  show (R (MAP mts) n trg c) = printf "MAP%s %s TARGET %s IF %s" (show n) (show mts)
                                                                 (show trg) (show c)
  show (R t n trg c) = printf "%s%s %s IF %s ;" (show t) (show n) (show trg) (show c)

--------------------------------------------------------------------------------
-- Tags and tagsets

data Tag = Tag String 
         | Lem String 
         | WF String 
         | Synt String
         | Subreading Subpos Tag 
         | Rgx String 
         | EOS | BOS deriving (Eq,Ord)

data Subpos = FromStart Int | FromEnd Int | Wherever

-- Specific structure for VISL CG-3 tag sets and lists.
-- It's polymorphic just so that I can use fmap and stuff.
data Set a = List (OrList (AndList a))    -- LIST Foo = foo ("<bar>" bar) baz
            | Union (Set a) (Set a)       -- SET Baz = Foo | Bar
            | Diff (Set a) (Set a)        -- SET Baz = Foo - Bar
            | Cart (Set a) (Set a)        -- SET Baz = Foo + Bar
--            | SymDif (Set a) (Set a)      -- SET Baz = Foo ∆ Bar
--            | Inters (Set a) (Set a)      -- SET Baz = Foo ∩ Bar
            | All deriving (Eq,Ord)  -- (*)

type TagSet = Set Tag

tagList :: Tag -> TagSet
tagList tag = List (Or [And [tag]])

bosSet :: TagSet
bosSet = tagList BOS

eosSet :: TagSet
eosSet = tagList EOS

--------------------------------------------------------------------------------

-- Following the conventions of vislcg3
instance Show Tag where
  show (Tag str) = str
  show (Synt str) = str -- I already include the @ in the name.
                        -- TODO if it turns out to be important, handle it nicer.
  show (Lem str) = printf "\"%s\"" str
  show (WF str)  = printf "\"<%s>\"" str
  show (Rgx str) =  printf "\"%s\"r" str
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

{- There is a lot of list action happening here, instead of in Rule.
   Why is that? It's because of NEGATE: it's meaningful to negate a
   linked context, or a template (named at least, maybe also inline?).
   There's still a top level AndList Context in the Rule data type:
   those can be anything specified here.
   REMOVE Foo IF (-1 Bar)  -- regular Ctx
                 (-2 Baz LINK 1 Quux LINK T:Xyzzy)  -- Link
                 (NEGATE T:tmpl) -- Negate + Template
                 ( (-1 Foo) OR (1 Bar) ); -- Inline template
   This rule has 4 contexts, and all of them must hold. -}
data Context = Ctx { position :: Position 
                   , polarity :: Polarity
                   , tags :: TagSet 
                   } 
             | Link (AndList Context)
             | Template (OrList Context) -- same for inline and named
             | Negate Context
             | Always deriving (Eq,Ord)

instance Show Context where
  show (Template cs) = show cs
  show (Negate ctx)  = "NEGATE " ++ show ctx
  show (Link cs)     = addParens $ intercalate " LINK " $ map singleCtx (getAndList cs)
  show Always        = []
  show ctx           = addParens (singleCtx ctx)

singleCtx (Ctx pos pol ts) = printf "%s%s %s" (show pol) (show pos) (show ts)
singleCtx x                = show x

data Position = Pos { scan :: Scan 
                    , careful :: Careful
                    , pos :: Int 
                    } deriving (Eq,Ord)

instance Show Position where
  show (Pos sc c p) = let (sc1,sc2) = showScan sc 
                      in printf "%s%s%s%s" sc1 (show p) (show c) sc2

data Scan = Exactly 
          | AtLeast 
          | Barrier TagSet 
          | CBarrier TagSet deriving (Eq,Ord)

instance Show Scan where
  show sc = let (a,b) = showScan sc in a++b

showScan :: Scan -> (String,String)
showScan Exactly = ([],[])
showScan AtLeast = ("*",[])
showScan (Barrier ts) = ("*", " BARRIER " ++ show ts)
showScan (CBarrier ts) = ("*", " CBARRIER " ++ show ts)


data Careful = C | NC deriving (Eq,Ord)

instance Show Careful where
  show C  = "C"
  show NC = []

data Polarity = Yes | Not deriving (Eq,Ord)

instance Show Polarity where
  show Yes = []
  show Not = "NOT "



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


newtype OrList a = Or { getOrList :: [a] } deriving (Eq,Ord,Functor,Foldable,Monoid)
newtype AndList a = And { getAndList :: [a] } deriving (Eq,Ord,Functor,Foldable,Monoid)

instance (Show a) => Show (AndList a) where
  show = unwords . map show . getAndList

instance {-# OVERLAPPABLE #-} (Show a) => Show (OrList a) where
  show = intercalate " OR " . map show . getOrList

instance {-# OVERLAPPABLE #-} Show (OrList (AndList Tag)) where
  show (Or tags) = addParens $ intercalate ")|(" (map show tags)

addParens x = "(" ++ x ++ ")"

--------------------------------------------------------------------------------

instance Functor Set where
  fmap f (List ts)      = List (fmap (fmap f) ts)
  fmap f (Union ts ts') = Union (fmap f ts) (fmap f ts')
  fmap f (Diff ts ts')  = Diff (fmap f ts) (fmap f ts')
  fmap f (Cart ts ts')  = Cart (fmap f ts) (fmap f ts')
  fmap f All            = All

instance (Show a) => Show (Set a) where
  show (List ts) = show ts
  show (Union ts ts') = show ts ++ " | " ++ show ts'
  show (Diff ts ts') = show ts ++ " - " ++ show ts'
  show (Cart ts ts') = show ts ++ " + " ++ show ts'
  show All = "(*)"


--------------------------------------------------------------------------------

--maybe this is not a good idea after all?
--instance Monoid (Set a) where
--  mempty = List (Or [And []])

--  mappend All _ = All
--  mappend _ All = All
--  mappend (List as) (List bs) = List (mappend as bs)
--  mappend as@(List _) (Cart ss ts) = Cart (mappend as ss) (mappend as ts)
--  mappend _ _ = undefined


{-

"<foo>" REMOVE bar IF baz 
 === REMOVE "<foo>" bar IF baz

 target = List (Or [And [Tag "bar"]])
 lemma =  List (Or [And [WF "foo"]])
 mappend lemma target = List (Or [And [Tag "bar", Lem "foo"]])

 target = Cart (List (Or [And ["bar"]])) 
               (List (Or [ And ["bar"], And ["hargle", "bargle"] ])) -- bar (hargle bargle)
 lemma =  List (Or [And [WF "foo"]])
 mappend lemma target =
          Cart (List (Or [And ["foo", "bar"]]))
               (List (Or [ And ["foo", bar"], And ["foo", "hargle", "bargle"] ] -- bar (hargle bargle)
                      )     
               )


-}


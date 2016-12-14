module Parse ( parseRules 
			 ) where

import qualified Rule as R
import CG.Abs
import CG.Lex
import CG.Par
import CG.Print
import CG.ErrM

import Control.Monad.State.Lazy
import Data.Either
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Regex.PCRE

data Env = Env { named :: [(String, R.TagSet)]
               , unnamed :: [R.TagSet] 
               , templates :: [(String, R.Context)] }

parseRules :: String -> String
parseRules = id
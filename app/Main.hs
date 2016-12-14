module Main where

import Rule
import Parse ( parseRules )

import System.Environment ( getArgs )

main :: IO ()
main = do args <- getArgs
          case args of
            [rls] -> print =<< fmap parseRules (readFile rls)
--            		     (parseRules rules)
            _     -> error "Usage: stack exec read-cg <rules.rlx>"


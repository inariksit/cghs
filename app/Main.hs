module Main where

import Rule
import Parse ( parse )

import System.Environment ( getArgs )

main :: IO ()
main = do args <- getArgs
          case args of
            [rls] -> do text <- readFile rls 
                        let (defs,rules) = parse True text
                        mapM_ print defs
                        putStrLn ""
                        mapM_ (mapM_ print) rules
            _     -> error "Usage: stack exec read-cg <rules.rlx>"


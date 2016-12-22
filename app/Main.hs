module Main where

import Rule
import Parse ( parse )

import System.Environment ( getArgs )

main :: IO ()
main = do args <- getArgs
          case args of
            [rls] -> do text <- readFile rls 
                        let text' = unlines $ map replaceComment $ lines text
                        let (defs,rules) = parse text'
                        mapM_ print defs
                        putStrLn ""
                        mapM_ (mapM_ print) rules
            _     -> error "Usage: stack exec read-cg <rules.rlx>"


replaceComment :: String -> String
replaceComment ('#':_) = []
replaceComment x       = x

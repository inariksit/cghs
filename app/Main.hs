module Main where

import CGHS ( Rule(..), TagSet, parse )
import System.Environment ( getArgs )


main :: IO ()
main = do args <- getArgs
          case args of
            [rls] -> do --E.setLocaleEncoding E.utf8 --maybe I'll need it later?
                        text <- readFile rls 
                        let (defs,rules) = parse text
                        mapM_ print defs
                        putStrLn ""
                        mapM_ (mapM_ print) rules
            _     -> error "Usage: stack exec read-cg <rules.rlx>"
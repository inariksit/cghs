module Main where

import Rule
import Parse ( parse )

import System.Environment ( getArgs )
--import qualified GHC.IO.Encoding as E


main :: IO ()
main = do args <- getArgs
          case args of
            [rls] -> do --E.setLocaleEncoding E.utf8 --maybe I'll need it later?
                        text <- readFile rls 
                        let (defs,rules) = parse text
                        mapM_ putStrLn defs
                        putStrLn ""
                        mapM_ (mapM_ print) rules
            _     -> error "Usage: stack exec read-cg <rules.rlx>"
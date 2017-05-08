module Main where

import CGHS ( Rule(..), TagSet, parse, printGrammar, groupRules, sortByContext, showInline )
import CGHS.Compact ( findRepetition, compactTagset, compactRule, compactStrings, groupRepetitiveTagset ) 
import System.Environment ( getArgs )


main :: IO ()
main = do args <- getArgs
          case args of
            [rls] -> do --E.setLocaleEncoding E.utf8 --maybe I'll need it later?
                        text <- readFile rls 
                        let compact = False

                        let gr = printGrammar compact text
                        putStrLn gr

                        let (defs,rules) = parse compact text
                        mapM_ (mapM_ print) (take 5 rules)
                        --mapM_ print defs
{-                        putStrLn ""
                        let repTsets = [ (def, groupRepetitiveTagset def)
                                         | (name,def) <- defs
                                         , not $ null (groupRepetitiveTagset def) ]                          
                        let show' = (\(x,y) -> "\nOriginal tagset:"
                                      ++ "\n----------------\n"
                                      ++ (show x ++ " = " ++ showInline x)
                                      ++ "\nPossibilities for regrouping:"
                                      ++ "\n-----------------------------\n"
                                      ++ unlines (map showInline y))
                        mapM_ (putStrLn.show') repTsets


                        --mapM_ (mapM_ print) rules
                        --mapM_ (\((nm,def),occs) -> case occs of
                        --         [] -> return ()
                        --         _  ->  do putStrLn "\nOriginal definition:"
                        --                   putStrLn (nm ++ " = " ++ show def)
                        --                   putStrLn "\nRepetitive elements:"
                        --                   mapM_ (\(x,y) -> 
                        --                          putStrLn (show x ++ ": " ++ show y)) occs
                        --                   putStrLn "-----\n"
                        --      ) repDefs

                        --putStrLn "\n\n\n"
                        --let compRules = map compactRule (concat rules)
                        --let diffRules = [ show orig ++ "\n\t" ++  show comp ++ "\n"
                        --                   | (orig,comp) <- zip (concat rules) compRules
                        --                   , orig /= comp ]                        
                        ----mapM_ putStrLn diffRules
                        --print (length diffRules)


                        putStrLn "Grouping rules by targets"
                        --let groupedRlsBySection = map groupRules rules 
                        let groupedRls = map ( sortByContext) $
                                 				  groupRules (concat rules) :: [[Rule]]
                        --print (elems groupledRls)
                        mapM_ (\x -> mapM_ print x >> putStrLn "\n") groupedRls
                        putStrLn "done"
-}
            _     -> error "Usage: stack exec read-cg <rules.rlx>"



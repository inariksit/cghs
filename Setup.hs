import Distribution.Simple
import System.Process ( callProcess )

main = do callProcess "bnfc"  ["-d", "src/bnfc/CG.cf"]
          callProcess "happy" ["-gca", "src/CG/Par.y"]
          callProcess "alex"  ["-g", "src/CG/Lex.x"]
          defaultMain

--main = defaultMainWithHooks simpleUserHooks { preConf = runBNFC }
-- where runBNFC = undefined

import Distribution.Simple
import System.Process ( callProcess )
import System.Directory ( withCurrentDirectory )

main = withCurrentDirectory "src" $
         do callProcess "bnfc"  ["-d", "bnfc/CG.cf"]
            callProcess "happy" ["-gca", "CG/Par.y"]
            callProcess "alex"  ["-g", "CG/Lex.x"]
--       defaultMain

--main = defaultMainWithHooks simpleUserHooks { preConf = runBNFC }
-- where runBNFC = undefined



import Distribution.Simple
import Distribution.Simple.Setup ( BuildFlags )
import Distribution.PackageDescription ( HookedBuildInfo , emptyHookedBuildInfo )

import System.Process ( callProcess )
import System.Directory ( withCurrentDirectory )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { preBuild = runBNFC }

runBNFC :: Args -> BuildFlags -> IO HookedBuildInfo
runBNFC _ _ = do withCurrentDirectory "src" $ do
                    callProcess "bnfc" ["-d", "bnfc/CG.cf"]
                    callProcess "happy" ["-gca", "CG/Par.y"]
                    callProcess "alex"  ["-g", "CG/Lex.x"]
                 return emptyHookedBuildInfo
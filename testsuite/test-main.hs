import System.Exit ( exitFailure, exitSuccess )
import Test.HUnit hiding ( counts )

import qualified Grads


main :: IO ()
main = do
   counts <- runTestTT tests
   exit $ testsPassed counts


exit :: Bool -> IO ()
exit True  = exitSuccess
exit False = exitFailure


testsPassed :: Counts -> Bool
testsPassed (Counts _ _ errs fails) = (errs == 0) && (fails == 0)


tests :: Test
tests = TestList
   [ Grads.tests
   ]

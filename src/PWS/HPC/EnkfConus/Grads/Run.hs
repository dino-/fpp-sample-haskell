module PWS.HPC.EnkfConus.Grads.Run
   ( runGrads )
   where

import Control.Concurrent.ParallelIO.Global ( parallel )
import Control.Monad ( (>=>) )
import System.Directory ( setCurrentDirectory )
import System.Exit ( ExitCode (ExitFailure))
--import System.FilePath
--import System.Process ( spawnCommand, system, waitForProcess )
import System.Process ( spawnCommand, waitForProcess )
import Text.Printf ( printf )

import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.Log
import PWS.HPC.EnkfConus.Trans ( GradsCmds (..), Images (..), Task (..), FPP, asks )


runGrads :: Task GradsCmds -> FPP (Task Images)
runGrads (Task (GradsCmds cmds)) = do
   intervalPath <- asks C.intervalPath

   liftIO $ do
      setCurrentDirectory intervalPath

      results <- parallel . map (spawnCommand >=> waitForProcess) $ cmds

      -- FIXME Revisit this later
      -- symlink all of the images we just created for this
      -- interval into the common images directory above
      --_ <- system $ printf "ln -s ../%s/images/* ../images/" intervalPath

      mapM_ reportFailure $ zip results cmds

   return (Task Images)


reportFailure :: (ExitCode, String) -> IO ()
reportFailure (ExitFailure code, cmd) =
   warningM lname $ printf "Non-zero exit code %d returned by this grads command:\n%s" code cmd
reportFailure _ = return ()


{- This emergency code using xargs does work too

runGrads :: Task GradsCmds -> FPP (Task ())
runGrads (Task (GradsCmds cmds)) = do
   intervalPath <- asks C.intervalPath

   liftIO $ do
      setCurrentDirectory intervalPath

      let cmdFilePath = intervalPath </> "grads_command_lines"

      let wrappedCmds = map (\cmd -> "'" ++ cmd ++ "'") cmds
      writeFile cmdFilePath $ unlines wrappedCmds

      ph <- spawnCommand $ printf "xargs -P 22 -a %s -L 1 sh -c" cmdFilePath
      waitForProcess ph

   return $ Task ()
-}

module PWS.HPC.EnkfConus.Grads.ResizeImages
   ( resizeImages )
   where

import Control.Concurrent.ParallelIO.Global ( parallel )
import Control.Monad ( (>=>) )
import Data.List ( isPrefixOf )
import System.Directory ( getDirectoryContents, setCurrentDirectory )
import System.Environment ( getEnv )
import System.Exit ( ExitCode (ExitFailure))
import System.FilePath ( (<.>), (</>) )
import System.Process ( spawnCommand, waitForProcess )
import Text.Printf ( printf )

import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.Log
import PWS.HPC.EnkfConus.Trans ( Images (..), Task (..), FPP, asks )


resizeImages :: Task Images -> FPP ()
resizeImages (Task Images) = do
   intervalPath <- asks C.intervalPath

   liftIO $ do
      setCurrentDirectory intervalPath

      -- Build a list of all image file names
      imageFiles <- filter (not . isPrefixOf ".") <$> getDirectoryContents "images"

      -- Build the pngquant script execution commands
      fppRoot <- getEnv "FPP_ENKFCONUS_ROOT"
      let scriptPath = fppRoot </> "bin" </> "img-resize" <.> "sh"
      let cmds = map (\img -> printf "%s %s" scriptPath img) imageFiles
      --mapM_ (debugM lname) cmds

      results <- parallel . map (spawnCommand >=> waitForProcess) $ cmds

      mapM_ reportFailure $ zip results cmds

   return ()


reportFailure :: (ExitCode, String) -> IO ()
reportFailure (ExitFailure code, cmd) =
   warningM lname $ printf "Non-zero exit code %d returned by this image resize command:\n%s" code cmd
reportFailure _ = return ()

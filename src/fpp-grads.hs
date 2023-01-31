import Control.Concurrent.ParallelIO.Global ( stopGlobalPool )

import PWS.HPC.EnkfConus.Config ( makeConfig )
import PWS.HPC.EnkfConus.Grads.Commands ( buildGradsCmds )
import PWS.HPC.EnkfConus.Grads.ResizeImages ( resizeImages )
import PWS.HPC.EnkfConus.Grads.Run ( runGrads )
import PWS.HPC.EnkfConus.Log ( initLogging, liftIO, lname, infoM, startLoggingM )
import PWS.HPC.EnkfConus.Trans ( runFPP )
import PWS.HPC.EnkfConus.Util ( exit )
import PWS.HPC.EnkfConus.WorkingDir ( makeImagesDir )


main :: IO ()
main = do
   -- Populate config from the environment
   conf <- makeConfig

   -- We need to do this very early because we log into this directory
   makeImagesDir conf

   -- Now that we have the config and a dir, we can initialize logging
   initLogging conf

   -- Log some start-up info
   startLoggingM "fpp-grads"
   infoM lname $ "Configuration: " ++ show conf

   -- Perform the post-processing work
   exit =<< (runFPP conf $ do
      buildGradsCmds
         >>= runGrads
         >>= resizeImages

      liftIO $ stopGlobalPool  -- Only after runGrads and resizeImages are complete
      )

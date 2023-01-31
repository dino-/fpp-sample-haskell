import Control.Concurrent.ParallelIO.Global ( stopGlobalPool )
import Control.Monad ( unless )

import PWS.HPC.EnkfConus.Config ( fosEnabled, makeConfig )
import PWS.HPC.EnkfConus.FilePost.Copy ( copyImages )
import PWS.HPC.EnkfConus.FilePost.Rest ( loadRunID, updateInterval )
import PWS.HPC.EnkfConus.Log ( initLogging, lname, infoM, liftIO, startLoggingM )
import PWS.HPC.EnkfConus.Trans ( runFPP )
import PWS.HPC.EnkfConus.Util ( exit )
import PWS.HPC.EnkfConus.WorkingDir ( makeFilePostDir )


main :: IO ()
main = do
   -- Populate config from the environment
   conf <- makeConfig

   makeFilePostDir conf

   -- Now that we have the config and a dir, we can initialize logging
   initLogging conf

   -- Log some start-up info
   startLoggingM "fpp-filepost"
   infoM lname $ "Configuration: " ++ show conf

   -- Check the config switch, if we shouldn't publish, get out of here NOW
   unless (fosEnabled conf) $ exit $ Right ()

   -- Perform the post-processing work
   exit =<< (runFPP conf $ do
      imgCopy <- loadRunID >>= copyImages
      liftIO $ stopGlobalPool  -- Only after all images are copied
      updateInterval imgCopy
      )

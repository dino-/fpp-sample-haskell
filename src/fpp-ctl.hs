import PWS.HPC.EnkfConus.Config ( makeConfig )
import PWS.HPC.EnkfConus.CtlIdx.CtlFile ( makeControlFile )
import PWS.HPC.EnkfConus.CtlIdx.IdxFile ( makeIndexFile )
import PWS.HPC.EnkfConus.FilePost.Rest ( addRun, storeRunID )
import PWS.HPC.EnkfConus.Log ( initLogging, lname, infoM, startLoggingM )
import PWS.HPC.EnkfConus.Trans ( runFPP )
import PWS.HPC.EnkfConus.Util ( exit )
import PWS.HPC.EnkfConus.WorkingDir ( makeIntervalDir )


main :: IO ()
main = do
   -- Populate config from the environment
   conf <- makeConfig

   -- We need to do this very early because we log into this directory
   makeIntervalDir conf

   -- Now that we have the config and a dir, we can initialize logging
   initLogging conf

   -- Log some start-up info
   startLoggingM "fpp-ctl"
   infoM lname $ "Configuration: " ++ show conf

   -- Let's make those ctl and idx files!
   --exit =<< (runFPP conf $ makeControlFile >>= makeIndexFile)
   exit =<< (runFPP conf $ do
      _ <- addRun >>= storeRunID

      -- FIXME makeControlFile should consume the output of storeRunID right?
      makeControlFile >>= makeIndexFile
      )

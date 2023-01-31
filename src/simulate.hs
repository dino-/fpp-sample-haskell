import Control.Monad ( forM_, when )
import System.Environment ( getArgs, setEnv )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath ( (</>) )
import System.Process ( system )
import Text.Printf ( printf )

import PWS.HPC.EnkfConus.Simulate.Opts ( Options (..), intervalRange, parseOpts, unHelp, usageText )


-- Note there are actually (optEndInterval + 1) intervals in total.


main :: IO ()
main = do
   (opts, args) <- parseOpts =<< getArgs
   when (unHelp . optHelp $ opts) $ putStrLn usageText >> exitSuccess
   when (length args < 2) $ putStrLn usageText >> exitFailure
   let (initTime : phase : []) = args

   setEnv "WEBGRADS_INITTIME" initTime

   let envir = show . optEnvir $ opts

   -- clean-up
   moduleCmd "rm fpp-enkf_conus"
   moduleCmd "rm lustre"

   setEnv "ENVIR" envir


   -- Outside of development, these three variables are set by ECFlow

   let lustreFS = show . optLustreFS $ opts
   setEnv "LUSTREFS" lustreFS

   let baseDirPart = "regional/enkf_conus"

   -- FIXME Must do something with these hard-coded paths!
   setEnv "PWS_R4K_BASE_DIR" $ lustreFS </> ("nw" ++ envir) </> baseDirPart
   -- Use this one to test run this script from prodX/devA as a normal user (for data writing perms)
   --setEnv "PWS_R4K_BASE_DIR"  $ "/cfs/devA/projects" </> "nwprod/global"

   setEnv "PWS_R4K_ALT_DIR" $ lustreFS </> ("nw" ++ (otherEnv envir)) </> baseDirPart
   -- Use this one to test run this script from prodX/devA as a normal user (for data writing perms)
   --setEnv "PWS_R4K_ALT_DIR"  $ "/cfs/devA/projects" </> "nwpara/global"


   -- Loading this here will prevent the fpp/VERSION module from loading it at all
   -- That allows us to slip our own value for PA_lustreDir into the env instead.
   -- This is how we run in .../projects/...
   moduleCmd "load lustre"
   setEnv "PA_lustreDir" lustreFS

   -- FIXME Exit with a proper code if any of these fail
   forM_ (intervalRange opts) $ \i -> do
      setEnv "WEBGRADS_INT" (show i)
      let partition = show . optPartition $ opts

      _ <- case phase of
         "ctl" -> system $ printf "sbatch -N 1 -p %s --share -J $WEBGRADS_INT.ctl $LUSTREFS/opt/fpp-enkf_conus/bin/enkf_conus-ctl.sh" partition
         "grads" -> system $ printf "sbatch -N 1 -p %s --exclusive -J $WEBGRADS_INT.grads $LUSTREFS/opt/fpp-enkf_conus/bin/enkf_conus-grads.sh" partition
         "filepost" -> do
            --let timeout = 1440  -- 24 hours

            --system $ printf "sbatch -N 1 -p %s --time=%d --share -J $WEBGRADS_INT.filepost $LUSTREFS/opt/fpp-enkf_conus/bin/enkf_conus-filepost.sh" partition (timeout :: Int)
            system $ printf "srun -N 1 -p %s --share -J $WEBGRADS_INT.filepost $LUSTREFS/opt/fpp-enkf_conus/bin/enkf_conus-filepost.sh" partition
         _ -> error $ printf "Can't continue because %s is not a valid phase." phase

      return ()


otherEnv :: String -> String
otherEnv "prod" = "para"
otherEnv "para" = "prod"
otherEnv _      = undefined


{- We are sometimes running this from a cron job, this helps with
   missing function definitions in inherited shell environments.
-}
moduleCmd :: String ->  IO ()
moduleCmd cmd = do
   _ <- system $ "/usr/bin/modulecmd bash " ++ cmd
   return ()

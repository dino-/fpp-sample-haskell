module PWS.HPC.EnkfConus.Simulate.Opts
   ( Options (..)
   , parseOpts, usageText
   , intervalRange
   , unHelp
   )
   where

import Data.Version ( showVersion )
import Paths_fpp_enkf_conus ( version )
import System.Console.GetOpt

import PWS.HPC.EnkfConus.Types ( Interval (..), Intervals )


maxInterval :: Interval
maxInterval = 144


newtype Start = Start Interval
newtype End = End Interval

newtype Envir = Envir String

instance Show Envir where
   show (Envir e) = e

newtype LustreFS = LustreFS FilePath

instance Show LustreFS where
   show (LustreFS p) = p

newtype Help = Help Bool

unHelp :: Help -> Bool
unHelp (Help b) = b

newtype Partition = Partition String

instance Show Partition where
   show (Partition p) = p

data Options = Options
   { optStartInterval :: Start
   , optEndInterval :: End
   , optEnvir :: Envir
   , optLustreFS :: LustreFS
   , optHelp :: Help
   , optPartition :: Partition
   }

defaultOptions :: Options
defaultOptions = Options
   { optStartInterval = Start 0
   , optEndInterval = End maxInterval
   , optEnvir = Envir "para"
   , optLustreFS = LustreFS "/cfs/devA/projects"
   , optHelp = Help False
   , optPartition = Partition "dev"
   }


intervalRange :: Options -> Intervals
intervalRange opts = [start..end] where
   Start start = optStartInterval opts
   End end = optEndInterval opts


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['s'] ["start-interval"]
      (ReqArg (\s opts -> opts { optStartInterval = Start . Interval . read $ s } ) "INT")
      "The starting interval. Defaults to 0"
   , Option ['e'] ["end-interval"]
      (ReqArg (\s opts -> opts { optEndInterval = End . Interval . read $ s } ) "INT")
      ("The ending interval. Defaults to " ++ (show maxInterval))
   , Option ['n'] ["envir"]
      (ReqArg (\s opts -> opts { optEnvir = Envir s } ) "prod|para")
      ("Value for the ENVIR environment variable. Defaults to " ++ (show . optEnvir $ defaultOptions))
   , Option ['l'] ["lustrefs"]
      (ReqArg (\s opts -> opts { optLustreFS = LustreFS s } ) "PATH")
      ("Value for the LUSTREFS environment variable. Defaults to " ++ (show . optLustreFS $ defaultOptions))
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = Help True } ))
      "This help text"
   , Option ['p'] ["partition"]
      (ReqArg (\s opts -> opts { optPartition = Partition s } ) "dev|mpidev|inputdata")
      ("Partition for submitting jobs. See PARTITION below. Defaults to " ++ (show . optPartition $ defaultOptions))
   ]


{- Perform the actual parse of a [String]
-}
parseOpts :: [String] -> IO (Options, [String])
parseOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: simulate [OPTIONS] INITTIME PHASE"
         , "Simulate a forecast post processing run for enkf_conus on the cluster by scheduling jobs with SLURM"
         , ""
         , "Options:"
         ]
      footer = init $ unlines
         [ "INITTIME sets the WEBGRADS_INITTIME environment variable. Value should be formatted YYYYMMDDHH  example: 2015120218"
         , "PHASE is the post-processing phase to run. One of ctl|grads|filepost"
         , ""
         , "For lustrefs, use /cfs/devA to run this script outside of /cfs/devA/projects. Say if you want to run against real data. Most uses of this utility will not need to change this value."
         , ""
         , "PARTITION"
         , ""
         , "The 'inputdata' partition is for the filepost phase specifically. It's seldom-used but will post to the production location."
         , ""
         , "Version " ++ (showVersion version)
         ]

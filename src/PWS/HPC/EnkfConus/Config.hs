module PWS.HPC.EnkfConus.Config
   ( Config (..)
   , makeConfig
   , constructIntervalDir
   , cyclePath
   , intervalPath
   , postWorkingPath
   )
   where

import Data.Time ( UTCTime )
import HSInstall ( getRsrcDir )
import Safe ( readNote )
import System.Environment ( getEnv )
import System.FilePath
import System.Log.Logger ( Priority (..) )
import Text.Printf ( printf )

import Paths_fpp_enkf_conus ( getDataDir )
import PWS.HPC.EnkfConus.Time
   ( Duration
   , computeValidDT
   , datefmt_fosDir
   , datefmt_netcdfCycle
   , datefmt_VDTName
   , parseInitTime
   )
import PWS.HPC.EnkfConus.Types ( Interval, intervalToFH, mkInterval )


data Config = Config
   { productName :: String
   , productId :: Int
   , domain :: Int
   , duration :: Duration
   , rsrcPath :: FilePath
   , baseDir :: FilePath
   , altDir :: FilePath
   , cycleDir :: FilePath
   , postWorkingDir :: FilePath
   , fosRunName :: String
   , fosHost :: String
   , fosPort :: Int
   , fosRestPath :: String
   , fosDestPrefix :: FilePath
   , fosDestPath :: FilePath
   , fosRestDelay :: Int
   , fosEnabled :: Bool
   , logPriority :: Priority
   , intervalDir :: FilePath
   , initTime :: UTCTime
   , validDT :: UTCTime
   , interval :: Interval
   }
   deriving Show


makeConfig :: IO Config
makeConfig = do
   let productName' = "enkf_conus"

   envir <- getEnv "ENVIR"
   cycleDirPrefix <- getEnv "CYCLE_DIR_PREFIX"
   it <- parseInitTime <$> getEnv "WEBGRADS_INITTIME"
   let prodDate = productName' <.> (datefmt_netcdfCycle it)
   let cycleDir' = cycleDirPrefix </> envir </> prodDate

   let dur = 30  -- minutes

   interval' <- either error id . mkInterval <$> readEnv "WEBGRADS_INT"

   let vdt = computeValidDT it $ intervalToFH interval'

   postWorkingDir' <- do
      postDir <- getEnv "POSTWORKING_DIR"
      return $ postDir </> prodDate

   let fosDestPath' = do
         dpPrefix <- getEnv "FOS_DEST_PATH"
         return $ dpPrefix </> (datefmt_fosDir it)

   fosEnabled' <- do
      val <- getEnv "FOS_ENABLED"
      return $ case val of
         "1" -> True
         _   -> False

   Config
      <$> (return productName')
      <*> (return 14)  -- product id
      <*> (return 0)  -- FIXME We're not doing domain yet with gfs
      <*> (return dur)
      <*> (getRsrcDir getDataDir)
      <*> (getEnv "FPP_BASE_DIR")
      <*> (getEnv "FPP_ALT_DIR")
      <*> (return cycleDir')
      <*> (return postWorkingDir')
      {- This is used to make the run name we pass to the FOS with the
         insert run REST call. Not necessarily the same string
         as productName.
      -}
      <*> (return "ENKF_CONUS")  -- FIXME Is this right?
      <*> (getEnv "FOS_HOST")
      <*> (readEnv "FOS_PORT")
      <*> (getEnv "FOS_REST_PATH")
      <*> (getEnv "FOS_DEST_PREFIX")
      <*> fosDestPath'
      <*> (readEnv "FOS_REST_DELAY")
      <*> (return fosEnabled')
      <*> (readEnv "LOGLEVEL")
      <*> (return $ constructIntervalDir postWorkingDir' vdt)
      <*> (return it)
      <*> (return vdt)
      <*> (return interval')


{- Make the working path for one individual forecast hour, below
   the postWorkingPath for this initTime
-}
constructIntervalDir :: FilePath -> UTCTime -> FilePath
constructIntervalDir postWorkingDir' vdt =
   postWorkingDir' </> datefmt_VDTName vdt


{- This function exists to construct a friendly error message
   when there's a problem accessing an environment variable

   It could be replaced with: `read <$> getEnv` if you don't care
   about nice errors. These functions are in the standard library.
-}
readEnv :: (Read a) => String -> IO a
readEnv varName = readNote
   (printf "contained in env variable %s" varName) <$>
   getEnv varName


cyclePath :: Config -> FilePath
cyclePath conf = (baseDir conf) </> (cycleDir conf)


intervalPath :: Config -> FilePath
intervalPath conf = (baseDir conf) </> (intervalDir conf)


postWorkingPath :: Config -> FilePath
postWorkingPath conf = (baseDir conf) </> (postWorkingDir conf)

module PWS.HPC.EnkfConus.Grads.Commands
   ( buildGradsCmds
   , formatFH
   )
   where

import           Data.List ( intercalate )
import qualified Data.Map as M
import           System.FilePath
import           Text.Printf ( printf )

import qualified PWS.HPC.EnkfConus.Config as C
import           PWS.HPC.EnkfConus.CtlIdx.CtlFile ( constructGribFileName )
import           PWS.HPC.EnkfConus.Grads.Param
import           PWS.HPC.EnkfConus.Grads.Region
import           PWS.HPC.EnkfConus.Log
import           PWS.HPC.EnkfConus.Time
                  ( computeValidDT
                  , datefmt_cycleOnly
                  , datefmt_gradsDate
                  , datefmt_gradsYear
                  )
import           PWS.HPC.EnkfConus.Trans ( GradsCmds (..), Task (..), FPP
                  , asks, tryIO )
import           PWS.HPC.EnkfConus.Types ( ForecastHour (..), Interval (..), intervalToFH )


buildGradsCmds :: FPP (Task GradsCmds)
buildGradsCmds = do
   liftIO $ infoM lname "Building grads command lines now.."

   rsrcPath    <- asks C.rsrcPath
   interval    <- asks C.interval

   params <- tryIO $ loadParams $ rsrcPath </> "grads-param"

   -- Filter for only the scripts that will be running for this interval
   let gradsRunning = M.filter (shouldGradRun interval) params

   regions <- tryIO $ loadRegions $ rsrcPath </> "grads-region"

   -- For every script left matched with every region, make a command line
   gradsCommands <- sequence [makeGradsCommandLine gp reg
         | gp <- M.toList gradsRunning, reg <- M.toList regions]

   -- Keep these things commented out unless debugging, they
   -- generate a lot of output
   --liftIO $ debugM lname $ "grads running for this interval:"
   --liftIO $ mapM_ (debugM lname) $ M.keys gradsRunning
   --liftIO $ mapM_ (debugM lname) gradsCommands

   liftIO $ infoM lname $
      printf "Constructed %d grads command lines for this FH"
         (length gradsCommands)

   return . Task . GradsCmds $ gradsCommands


shouldGradRun :: Interval -> Param -> Bool
shouldGradRun (Interval runInt) (Param _ (Interval startInt) freq _)
   = afterOrOnStartInt && correctFrequency

   where
      -- Is current run forecast hour after or on the grads script's target start forecast hour?
      afterOrOnStartInt = runInt >= startInt

      -- Is current run forecast hour one that this grad runs on at all?
      correctFrequency = (snd $ divMod runInt freq) == 0


{- Deal with ridiculous number formatting requirements for forecast hours

   1.0   -> "01"
   2.5   -> "02.5"
   10.0  -> "10"
   12.5  -> "12.5"
   100.0 -> "100"
   120.5 -> "120.5"
-}
formatFH :: ForecastHour -> String
formatFH (ForecastHour fh) = printf fmtString fh where
   fmtString = if (snd $ ((properFraction fh) :: (Int, Double))) == 0.0
      then "%02.0f"
      else "%04.1f"
--formatFH = printf "%06.2f"


{- Given our config, grad script info and a region, build a
   command-line to send to the grads utility
-}
makeGradsCommandLine :: (String, Param) -> (String, Region) -> FPP String
makeGradsCommandLine
   (_, Param gradScriptName _ _ _)
   (regionName, Region _ argsA argsB) = do

   forecastHourFormatted <- formatFH . intervalToFH <$> asks C.interval

   let logFilename = (printf "grads.%s.%s.f%s.log" regionName gradScriptName
         forecastHourFormatted) :: String

   printf "grads -blcx \"run %s %s %s %s %s %s %s %s %s %s %s %s\" > %s"
      <$> ((</> "grads" </> gradScriptName <.> "gs") <$> asks C.rsrcPath)
      <*> (return argsA)
      <*> (return forecastHourFormatted)
      <*> (return forecastHourFormatted)
      <*> (datefmt_gradsDate <$> asks C.initTime)
      <*> (datefmt_cycleOnly <$> asks C.initTime)
      <*> (datefmt_gradsDate <$> asks C.validDT)
      <*> (datefmt_cycleOnly <$> asks C.validDT)
      <*> (datefmt_gradsYear <$> asks C.initTime)
      <*> (return regionName)
      <*> (return argsB)
      <*> buildCtlPaths
      <*> (return logFilename)


buildCtlPaths :: FPP String
buildCtlPaths = do
   -- We need the current FH we're building a grads command for
   fh <- intervalToFH <$> asks C.interval

   -- This is a list of forecast hours for which we need ctl files (including this FH)
   let cfhList = filter (>= 0) [fh, fh - 1, fh - 3, fh - 6, fh - 24, fh - 48]

   -- This is a list of ctl file paths for each of those hours, for each ctlType
   ctlPaths <- sequence [makeCtlPath cfh | cfh <- cfhList]

   -- Make those into one space-delimited string
   return . intercalate " " $ ctlPaths


makeCtlPath :: ForecastHour -> FPP FilePath
makeCtlPath fh = do
   initTime          <- asks C.initTime
   baseDir           <- asks C.baseDir
   postWorkingDir    <- asks C.postWorkingDir

   let vdt = computeValidDT initTime fh

   let ctlPath = baseDir </> C.constructIntervalDir postWorkingDir vdt

   gribFileName <- constructGribFileName fh
   return $ (ctlPath </> gribFileName <.> "ctl")

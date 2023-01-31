{-# LANGUAGE FlexibleContexts #-}

module PWS.HPC.EnkfConus.CtlIdx.CtlFile
   ( constructGribFileName
   , makeControlFile
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad.Except ( MonadError, MonadIO, throwError )
import Data.Char ( toLower )
import System.Directory ( doesFileExist )
import System.FilePath
import System.Posix.Files ( fileSize, getFileStatus )
import System.Posix.Types ( COff )
import Text.Printf ( printf )

import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.Log
import PWS.HPC.EnkfConus.Time
   ( computeValidDT
   , datefmt_tdef
   , datefmt_VDTName
   )
import PWS.HPC.EnkfConus.Trans ( Ctl (..), Task (..), FPP, asks, tryIO )
import PWS.HPC.EnkfConus.Types ( ForecastHour, intervalToFH )


makeControlFile :: FPP (Task Ctl)
makeControlFile = do
   -- We'll need these values from the environment in several
   -- places below
   productName    <- asks C.productName
   rsrcPath       <- asks C.rsrcPath
   cyclePath      <- asks C.cyclePath
   intervalPath   <- asks C.intervalPath
   forecastHour   <- intervalToFH <$> asks C.interval
   gribFileName   <- constructGribFileName forecastHour

   -- Continue only after the grib file is present
   waitForFile 30 2 (cyclePath </> gribFileName) 50000000

   -- Build up the ctl file template path
   let ctlTemplatePath =
         rsrcPath </> productName <.> "wrfprs.ctl" <.> "template"

   -- Load the template in
   ctlTemplate <- tryIO . readFile $ ctlTemplatePath

   -- Values we need for the substitutions
   let ctlFileName   =  gribFileName <.> "ctl"
   let idxFileName   =  gribFileName <.> "idx"
   tdefDate          <- datefmt_tdef <$> (asks C.validDT)
   let tdefThingy    =  if forecastHour <= 24 then "1hr" else "3hr"

   -- Make the new file contents
   let ctlContents = printf ctlTemplate (cyclePath </> gribFileName)
         idxFileName gribFileName tdefDate tdefThingy

   let ctlFilePath = intervalPath </> ctlFileName
   tryIO $ writeFile ctlFilePath ctlContents
   liftIO $ noticeM lname $ "ctl file created: " ++ ctlFilePath

   return $ Task (Ctl gribFileName)



constructGribFileName :: ForecastHour -> FPP FilePath
constructGribFileName fh = do
   productName <- asks C.productName
   validDT <- computeValidDT <$> asks C.initTime <*> return fh
   return $ printf "%s_d02.%s.wrfprs.grib2" (map toLower productName)
      (datefmt_VDTName validDT)


{- Wait for a file to be present and to be greater than the specified size
-}
waitForFile :: (MonadError String m, MonadIO m)
   => Int -> Int -> FilePath -> COff -> m ()
waitForFile loops sleepTime filePath targetSize = waitForFile' loops
   where
      waitForFile' :: (MonadError String m, MonadIO m) => Int -> m ()
      waitForFile' 0 = throwError $ "Cannot continue because required file is missing: " ++ filePath
      waitForFile' n = do
         exists <- liftIO $ doesFileExist filePath
         result <- if exists
            then do
               actualSize <- liftIO $ fileSize <$> getFileStatus filePath
               if (actualSize < targetSize)
                  then return $ Left $ printf "File %s not yet complete, waiting..." filePath
                  else return $ Right ()
            else return $ Left $ printf "File %s not yet present, waiting..." filePath

         case result of
            Right _ -> return ()
            Left msg -> do
               liftIO $ warningM lname msg
               liftIO $ threadDelay (sleepTime * 1000 * 1000)
               waitForFile' (n - 1)

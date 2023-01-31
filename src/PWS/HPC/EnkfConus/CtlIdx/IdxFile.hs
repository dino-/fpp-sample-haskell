module PWS.HPC.EnkfConus.CtlIdx.IdxFile ( makeIndexFile )
   where

import Control.Monad ( unless )
import System.Directory ( doesFileExist, setCurrentDirectory )
import System.FilePath
import System.Process ( readProcess )

import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.Log
import PWS.HPC.EnkfConus.Trans ( Ctl (..), Task (..), FPP, asks, throwError, tryIO )


makeIndexFile :: Task Ctl -> FPP ()
makeIndexFile (Task (Ctl gribFileName)) = do
   intervalPath <- asks C.intervalPath

   tryIO $ do
      setCurrentDirectory intervalPath
      readProcess "gribmap" ["-i", gribFileName <.> "ctl"] "" >>= infoM lname

   -- gribmap doesn't actually fail if the idx file is not made, so..
   idxExists <- liftIO $ doesFileExist $ gribFileName <.> "idx"
   unless idxExists $ throwError "idx file was not created"

   liftIO $ noticeM lname $ "idx file created"

{-# LANGUAGE EmptyDataDecls #-}

module PWS.HPC.EnkfConus.Trans
   ( AddRun (..), Ctl (..), GradsCmds (..), ImageCopy (..), Images (..)
   , RunStored (..), Task (..)
   , FPP, runFPP
   , tryIO

   -- Re-exporting from Except
   , throwError

   -- Re-exporting from Reader
   , ask, asks
   )
   where

import Control.Monad.Catch ( catchIOError )
import Control.Monad.Except
import Control.Monad.Reader

import PWS.HPC.EnkfConus.Config ( Config )


{- The broad things this code does (control/index files, grads script
   runs, file post) are broken down further into what we call "Tasks"

   This type represents a task for a given "phase"
-}
data Task phase = Task phase


-- These are the phases

-- This one means a control file has been made successfully
data Ctl = Ctl
   FilePath -- grib file name

-- This phase means grads commands have been constructed
data GradsCmds = GradsCmds
   [String]  -- Commands to execute all grads scripts for a specific FH

-- This phase means grads have run and images have been created
data Images = Images

-- This phase means a file post run has been created successfully
data AddRun = AddRun
   Int  -- runID

-- This phase signified that the run ID has been stored on disk
data RunStored = RunStored

-- This phase means that images have been copied for the portal
data ImageCopy = ImageCopy
   Int  -- runID


{- This type and function wrap around the monad transformer stack
   we're using for most of our top-level computations in this program
-}

type FPP a = ReaderT Config (ExceptT String IO) a

runFPP :: Config -> FPP a -> IO (Either String a)
runFPP env action = runExceptT $ runReaderT action env


{- Convenience wrapper for rethrowing instances of IOError
   catchIOError comes from the `exceptions` package
-}
tryIO :: IO a -> FPP a
tryIO act = catchIOError (liftIO act) $ throwError . show

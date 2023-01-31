module PWS.HPC.EnkfConus.CtlIdx.ItagFile
   where

import Prelude hiding ( cycle )
import System.IO ( IOMode (WriteMode), hPutStrLn, withFile )
import Text.Printf ( printf )

import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.Time
   ( datefmt_netcdfCycle
   , datefmt_tdef
   , datefmt_VDTName
   , datefmt_wrfout
   )
import PWS.Netops.FPP.Common ( FPP, asks, tryIO )


mkItag :: FPP ()
mkItag = do
   postWorkingDir <- asks $ C.postWorkingDir . config
   utCycle <- asks cycle
   cycleDirTmpl <- asks $ C.cycleDir . config
   domain <- asks $ C.domain . config
   validdt <- asks validDT

   let cycleDir = (printf cycleDirTmpl $ datefmt_netcdfCycle utCycle)
         :: String
   let wrfoutWithColons = datefmt_wrfout validdt
   let netcdfFQP = printf "%s/wrfout_d%02d_%s" cycleDir (domain :: Int)
         wrfoutWithColons

   tryIO $ withFile (postWorkingDir </> "itag") WriteMode $ \handle -> do
      hPutStrLn handle netcdfFQP
      hPutStrLn handle "netcdf"
      hPutStrLn handle wrfoutWithColons
      hPutStrLn handle "NCAR"

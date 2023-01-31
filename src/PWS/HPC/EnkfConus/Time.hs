module PWS.HPC.EnkfConus.Time
   where

import Data.Time ( UTCTime (..), addUTCTime, formatTime )
import Data.Time.Format ( defaultTimeLocale, parseTimeOrError )

import PWS.HPC.EnkfConus.Types ( ForecastHour (..) )


{- Various date formatters we use everywhere
-}

datefmt_netcdfCycle, datefmt_cycleDate, datefmt_cycleOnly
   , datefmt_tdef , datefmt_VDTName, datefmt_wrfout
   , datefmt_gradsDate , datefmt_gradsYear, datefmt_fosDir
   :: UTCTime -> String

datefmt_netcdfCycle = formatTime defaultTimeLocale "%Y%m%d.%H"

datefmt_cycleDate = formatTime defaultTimeLocale "%Y%m%d"
datefmt_cycleOnly = formatTime defaultTimeLocale "%H"

-- Used in ctl files for the tdef parameter
datefmt_tdef    = formatTime defaultTimeLocale "%HZ%d%b%Y"

-- Used for constructing grib, ctl and idx file names
datefmt_VDTName = formatTime defaultTimeLocale "%Y%m%d_%H%M"

-- Used for the itag file contents
datefmt_wrfout = formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S"

-- For grads scripts
datefmt_gradsDate = formatTime defaultTimeLocale "%m/%d/%Y"
datefmt_gradsYear = formatTime defaultTimeLocale "%Y"

-- For the FOS directory names
datefmt_fosDir = formatTime defaultTimeLocale datepat_initTime


{- Parse one of our human-readable init times into a UTCTime so
   we can format it into other things later
-}
datepat_initTime :: String
datepat_initTime = "%Y%m%d%H"

parseInitTime :: String -> UTCTime
parseInitTime = parseTimeOrError False defaultTimeLocale datepat_initTime


type Duration = Int


{- Compute the absolute interval time knowing the inittime, duration
   and interval number. The duration is constant across all intervals.
-}
computeValidDT :: UTCTime -> ForecastHour -> UTCTime
computeValidDT initTime (ForecastHour forecastHour) =
   addUTCTime intervalSeconds initTime
   where
      intervalMinutes = realToFrac $ 60 * forecastHour
      intervalSeconds = intervalMinutes * 60

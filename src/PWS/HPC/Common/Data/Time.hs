module PWS.HPC.Common.Data.Time
   ( milliToUTCTime
   , utcTimeToMilli
   )
   where

import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )


{- Parse a Double representing the time in milliseconds into a
   Haskell UTCTime data structure

   FIXME This should really take an Int or Integer for input. WTF?
-}
milliToUTCTime :: Double -> UTCTime
milliToUTCTime = posixSecondsToUTCTime . realToFrac
   . (/ (1000 :: Double))


{- Convert a UTCTime into milliseconds since epoch
-}
utcTimeToMilli :: UTCTime -> Int
utcTimeToMilli = (* (1000)) . round . utcTimeToPOSIXSeconds

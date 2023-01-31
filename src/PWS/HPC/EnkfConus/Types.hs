{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PWS.HPC.EnkfConus.Types
   ( Interval ( Interval, unInterval ), Intervals
   , ForecastHour (..), ForecastHours
   , fhToInterval , intervalToFH
   , mkInterval
   )
   where

import Text.Printf ( printf )


newtype Interval = Interval { unInterval :: Int }
   deriving (Enum, Eq, Num, Ord)

instance Show Interval where
   show (Interval i) = show i

instance Read Interval where
   readsPrec d s = [ (Interval i, s') | (i, s') <- readsPrec d s ]

type Intervals = [Interval]

newtype ForecastHour = ForecastHour Double
   deriving (Eq, Num, Ord)

type ForecastHours = [ForecastHour]


intervalMin, intervalMax :: Interval
intervalMin = Interval 0
intervalMax = Interval 144


mkInterval :: Int -> Either String Interval
mkInterval i
   | (intervalMin <= proposedInterval) &&
     (proposedInterval <= intervalMax) = Right proposedInterval
   | otherwise = Left $
      printf "Could not create Interval because %d is outside the range from %s to %s"
         i (show intervalMin) (show intervalMax)

   where
      proposedInterval = Interval i


{- Turn a forecast hour into an interval
-}
fhToInterval :: ForecastHour -> Interval
fhToInterval (ForecastHour fh) = Interval . round $ fh * 2


{- Turn an interval into a forecast hour
-}
intervalToFH :: Interval -> ForecastHour
intervalToFH (Interval interval) = ForecastHour $ (fromIntegral interval) / 2


module Grads
   ( tests )
   where

import System.FilePath ( (</>) )
import Test.HUnit

import PWS.HPC.EnkfConus.Grads.Commands ( formatFH )
import PWS.HPC.EnkfConus.Grads.Param ( getParamID, loadParams )
import PWS.HPC.EnkfConus.Types ( ForecastHour (..) )


tests :: Test
tests = TestList
   [ testGradsDBID
   , testGradsImageFHFormat
   ]


{- Test that a given grads image filename produces the correct database ID
-}
testGradsDBID :: Test
testGradsDBID = TestCase $ do
   let testData =
         [ ( 47, "1000to500.thick-mslp.8.5.png")
         , (131, "1000to850.thick-mslp.8.5.png")
         , ( 91, "250.hgt_m-wind_kts.8.5.png")
         , ( 90, "500.hgt-wnd.8.5.png")
         , ( 85, "700.rh.8.5.png")
         , (132, "850.tmp.8.5.png")
         , (128, "surface.dwpt_F.8.5.png")
         , ( 97, "surface.gust_kts.8.5.png")
         , ( 38, "surface.reflect_dbz.8.5.png")
         , ( 94, "surface.tmp_F.8.5.png")
         , ( 95, "surface.wind_kts.8.5.png")
         ]

   params <- loadParams ("resources" </> "grads-param")

   mapM_ (\(fos, filePath) ->
      assertEqual filePath (Just fos) (getParamID params filePath)) testData


testGradsImageFHFormat :: Test
testGradsImageFHFormat = do
   let expected =
         [ "01"
         , "02.5"
         , "10"
         , "12.5"
         , "100"
         , "120.5"
         ]

   let actual =
         [ formatFH (ForecastHour 1.0)
         , formatFH (ForecastHour 2.5)
         , formatFH (ForecastHour 10.0)
         , formatFH (ForecastHour 12.5)
         , formatFH (ForecastHour 100.0)
         , formatFH (ForecastHour 120.5)
         ]

   expected ~=? actual

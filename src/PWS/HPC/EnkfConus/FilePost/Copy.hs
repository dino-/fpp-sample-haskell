module PWS.HPC.EnkfConus.FilePost.Copy
   ( copyImages
   )
   where

import Control.Concurrent.ParallelIO.Global ( parallel )
--import Control.Monad ( (>=>) )
import Data.List ( isPrefixOf )
import System.Directory ( copyFile, getDirectoryContents )
--import System.Directory ( setCurrentDirectory )
--import System.Exit ( ExitCode (ExitFailure))
import System.FilePath
--import System.Process ( spawnCommand, system, waitForProcess )
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )

import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.FilePost.Rest ( addFile )
import PWS.HPC.EnkfConus.Grads.Param
import PWS.HPC.EnkfConus.Grads.Region
import PWS.HPC.EnkfConus.Log
import PWS.HPC.EnkfConus.Trans ( AddRun (..), ImageCopy (..), Task (..), FPP, ask, asks, tryIO )


copyImages :: Task AddRun -> FPP (Task ImageCopy)
copyImages (Task (AddRun runID)) = do
   -- src and dest directories
   imgDir <- (</> "images") <$> asks C.intervalPath
   fosDestPrefix <- asks C.fosDestPrefix
   fosDestPath <- asks C.fosDestPath

   -- Need a list of all image files for this interval
   imgFiles <- liftIO
      $ filter (not . isPrefixOf ".")  -- ..minus the dotfiles
      <$> getDirectoryContents imgDir  -- Contents of the images directory..
   liftIO $ debugM lname $ printf "Number of images to file post: %d" (length imgFiles)

   conf <- ask
   rsrcPath <- asks C.rsrcPath
   params <- tryIO $ loadParams $ rsrcPath </> "grads-param"
   regions <- tryIO $ loadRegions $ rsrcPath </> "grads-region"

   liftIO $ debugM lname "About to copy images"
   {-
   liftIO $ mapM_
      (\f -> copyImage conf runID params regions imgDir fosDestPrefix fosDestPath f)
      $ imgFiles
   -}

   _ <- liftIO $ do
      results' <- parallel . (map
         (\f -> copyImage conf runID params regions imgDir fosDestPrefix fosDestPath f))
         $ imgFiles
      --stopGlobalPool  -- FIXME Warning, this can only be done ONCE
      return results'

   --liftIO $ debugM lname $ "result: " ++ (show . head $ results)
   liftIO $ debugM lname "Done with copy images"

   return $ Task $ ImageCopy runID


copyImage :: C.Config -> Int -> Params -> Regions -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
copyImage conf runID params regions srcDir destPrefix destDir fileName = do
   copyFile (srcDir </> fileName) (destPrefix </> destDir </> fileName)

   let matchResults = matchRegex (mkRegex "^([^.]+)\\.(.+)") fileName
   let (region, grimg) = case matchResults of
         Just (region' : grimg' : []) -> (region', grimg')
         _                             -> error $ "Cannot construct file post REST call because of unparseable image filename: " ++ fileName

   debugM lname $ "region: \"" ++ region ++ "\""
   debugM lname $ "grimg extracted with regexp: \"" ++ grimg ++ "\""

   let paramID = maybe (error $ "Bad grads image string: " ++ grimg) id $ getParamID params grimg
   let regionID = maybe (error $ "Bad region string: " ++ region) id $ getRegionID regions region

   addFile conf runID paramID regionID fileName destDir


{-
runGrads :: Task GradsCmds -> FPP (Task ())
runGrads (Task (GradsCmds cmds)) = do
   intervalPath <- asks C.intervalPath

   liftIO $ do
      setCurrentDirectory intervalPath

      results <- parallel . map (spawnCommand >=> waitForProcess) $ cmds
      stopGlobalPool

      -- Hard-link all of the images we just created for this
      -- interval into the common images directory above
      _ <- system $ printf "ln ./images/* ../images/"

      mapM_ reportFailure $ zip results cmds

   return $ Task ()


reportFailure :: (ExitCode, String) -> IO ()
reportFailure (ExitFailure code, cmd) =
   warningM lname $ printf "Non-zero exit code %d returned by this grads command:\n%s" code cmd
reportFailure _ = return ()
-}

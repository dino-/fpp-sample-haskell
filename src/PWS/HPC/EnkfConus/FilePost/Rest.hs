module PWS.HPC.EnkfConus.FilePost.Rest
   ( addRun, addFile
   , updateInterval
   , fhToInterval
   , storeRunID
   , loadRunID
   )
   where

import Control.Concurrent ( threadDelay )
import Control.Monad ( when )
import Control.Monad.Catch ( catchIOError )
import Data.Char ( toUpper )
import Network.HTTP
import System.Directory ( doesDirectoryExist )
import System.FilePath ( (</>) )
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )

import PWS.HPC.Common.Data.Time ( utcTimeToMilli )
import qualified PWS.HPC.EnkfConus.Config as C
import PWS.HPC.EnkfConus.Log
import PWS.HPC.EnkfConus.Time
   ( datefmt_cycleDate
   , datefmt_cycleOnly
   )
import PWS.HPC.EnkfConus.Trans ( AddRun (..), ImageCopy (..), RunStored (..), Task (..), FPP
   , ask, asks, throwError, tryIO )
import PWS.HPC.EnkfConus.Types ( Interval ( unInterval ) )
import PWS.HPC.EnkfConus.Util ( createSymlinkIfMissing )


runIDFileName :: FilePath
runIDFileName = "runid"


makeRestCall :: C.Config -> String -> IO (Either String String)
makeRestCall conf body = do
   let fosHost = C.fosHost conf
   let fosPort = C.fosPort conf
   let fosRestPath = C.fosRestPath conf
   let fosRestDelay = C.fosRestDelay conf

   let url = printf "http://%s:%d/%s" fosHost fosPort fosRestPath

   noticeM lname $ printf "Making a REST call now to %s" url
   noticeM lname $ printf "with body:\n%s" body

   response <- tryRestCall 3 url body fosRestDelay

   debugM lname $ "response:\n" ++ (show response)

   return response


tryRestCall :: Int -> String -> String -> Int -> IO (Either String String)

tryRestCall 0         url body _     = do
   return $ Left $ printf "Unable to make REST call! to url:\n%s\nwith body:\n%s" url body

tryRestCall tryNumber url body delay = do
   let act = Right <$> (simpleHTTP (postRequestWithBody url "text/xml" body) >>= getResponseBody)
   response <- catchIOError act (return . Left . show)
   case response of
      Right r -> return . Right $ r
      Left e -> do
         warningM lname $ show e
         threadDelay (delay * 1000 * 1000)  
         tryRestCall (tryNumber - 1) url body delay


addRun :: FPP (Task (Maybe AddRun))
addRun = do
   fosRunName  <- asks C.fosRunName
   productId   <- asks C.productId
   initTime    <- asks C.initTime
   interval    <- asks C.interval

   if (interval == 0)
      then do
         let name = printf "PWS %s\\@%s %sz" (map toUpper fosRunName)
               (datefmt_cycleDate initTime) (datefmt_cycleOnly initTime)

         let body = printf "<insert><run><name>%s</name><init_time>%d</init_time><products><product>%d</product></products></run></insert>" (name :: String) (utcTimeToMilli initTime) productId

         conf <- ask
         ereply <- liftIO $ makeRestCall conf body

         reply <- either (throwError . show) return ereply

         let mbRunID = matchRegex (mkRegex ".*id>([0-9]+)</id.*") reply

         runID <- maybe
               (throwError $ "Can't find run ID because response is unparseable: " ++ reply)
               (return . read . head) mbRunID

         liftIO $ infoM lname $ "Extracted run id: " ++ (show runID)

         return . Task . Just . AddRun $ runID
      else return $ Task Nothing


storeRunID :: Task (Maybe AddRun) -> FPP (Task RunStored)
storeRunID (Task Nothing) = return $ Task RunStored
storeRunID (Task (Just (AddRun runID))) = do
   baseDir <- asks C.baseDir
   altDir <- asks C.altDir
   postWorkingDir <- asks C.postWorkingDir

   -- Make the run ID file and symlink it into the alt dir
   tryIO $ do
      let runIDPath = baseDir </> postWorkingDir </> runIDFileName

      -- Write the file first
      writeFile runIDPath $ show runID

      -- Now symlink it into the alt dir
      altDirExists <- doesDirectoryExist altDir
      when altDirExists $
         createSymlinkIfMissing runIDPath (altDir </> postWorkingDir </> runIDFileName)

   return $ Task RunStored


loadRunID :: FPP (Task AddRun)
loadRunID = do
   baseDir <- asks C.baseDir
   postWorkingDir <- asks C.postWorkingDir
   tryIO $ Task . AddRun . read <$> readFile (baseDir </> postWorkingDir </> runIDFileName)


addFile :: C.Config -> Int -> Int -> Int -> FilePath -> FilePath -> IO ()
addFile conf runID paramID regionID fileName destDir = do
   let initTime = C.initTime conf
   let validDT = C.validDT conf
   let duration = C.duration conf
   --let fh = C.forecastHour conf
   let interval = unInterval . C.interval $ conf

   let body = printf "<insert><file><init_time>%d</init_time><valid_datetime>%d</valid_datetime><priority>1</priority><filename>%s</filename><filepath>%s</filepath><run_id>%d</run_id><parameter_id>%d</parameter_id><region_id>%d</region_id><filetype_id>1</filetype_id><intervalduration>%d</intervalduration><intervalnumber>%d</intervalnumber><storm_id>0</storm_id></file></insert>"
         (utcTimeToMilli initTime)
         (utcTimeToMilli validDT)
         fileName
         destDir
         runID
         paramID
         regionID
         duration
         --(fhToInterval fh)
         interval

   _ <- makeRestCall conf body

   return ()


updateInterval :: Task ImageCopy -> FPP ()
updateInterval (Task (ImageCopy runID)) = do
   let body = printf "<update><availableinterval><run_id>%d</run_id></availableinterval></update>" runID
   liftIO $ debugM lname $ "updateInterval REST call body:\n" ++ body

   conf <- ask

   ereply <- liftIO $ makeRestCall conf body
   reply <- either (throwError . show) return ereply
   liftIO $ debugM lname $ "updateInterval REST call reply:\n" ++ reply

   return ()


fhToInterval :: Int -> Int
fhToInterval fh
   | fh < 24 = fh
   | otherwise = ((fh - 24) `div` 3) + 24

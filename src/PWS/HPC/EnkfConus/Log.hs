{- |
   An example of very simple hslogger usage with these small helper
   definitions.

>     import PWS.HPC.EnkfConus.Log
>
>     initLogging someConfig
>     noticeM lname "A log message"
>     debugM lname "Another log message"

-}

module PWS.HPC.EnkfConus.Log
   ( initLogging, lname
   , startLoggingM

   -- Re-exported from System.Log
   , debugM, infoM, noticeM, warningM, errorM , criticalM, alertM, emergencyM

   -- Re-exported from Control.Monad.Trans
   , liftIO
   )
   where

import Control.Monad.Trans ( liftIO )
import Data.Version ( showVersion )
import Paths_fpp_enkf_conus ( version )
import System.FilePath
import System.IO ( stdout )
import System.Log.Formatter ( simpleLogFormatter )
import System.Log.Handler ( LogHandler, setFormatter )
import System.Log.Handler.Simple ( fileHandler, streamHandler )
import System.Log.Logger
import Text.Printf ( printf )

import PWS.HPC.EnkfConus.Config ( Config (..), intervalPath )


lname :: String
lname = rootLoggerName


initLogging :: Config -> IO ()
initLogging conf = do
   let logFilePath = (intervalPath conf)
         </> ("fpp-" ++ (productName conf)) <.> "log"

   -- Remove the root logger's default handler that writes every
   -- message to stderr!
   updateGlobalLogger lname removeHandler
   updateGlobalLogger lname $ setLevel $ logPriority conf

   addHandler' $ streamHandler stdout
   addHandler' $ fileHandler logFilePath


addHandler' :: (LogHandler l) => (Priority -> IO l) -> IO ()
addHandler' handler =
   (flip setFormatter $ simpleLogFormatter "[$time : $prio] $msg")
      <$> handler DEBUG >>= updateGlobalLogger lname . addHandler


lineM :: IO ()
lineM = noticeM lname $ replicate 60 '-'


startLoggingM :: String -> IO ()
startLoggingM progName = do
   lineM
   noticeM lname $
      printf "%s version %s started" progName (showVersion version)

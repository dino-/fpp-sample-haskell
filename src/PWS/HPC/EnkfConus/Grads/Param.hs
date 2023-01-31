module PWS.HPC.EnkfConus.Grads.Param
   ( Params, Param (..)
   , loadParams
   , getParamID
   )
   where

import Control.Monad ( mplus )
import Data.List ( isPrefixOf )
import qualified Data.Map as M
import Safe ( readNote )

import PWS.HPC.EnkfConus.Types ( Interval )
import PWS.HPC.EnkfConus.Util ( removeComments )


type Params = M.Map ImageName Param

type ImageName = String

data Param = Param GradsScriptName StartInterval Frequency FOS
   deriving (Read, Show)

type GradsScriptName = String

type StartInterval = Interval

type Frequency = Int

type FOS = Int


loadParams :: FilePath -> IO Params
loadParams path = M.fromList . readNote ("ERROR parsing param file " ++ path)
   . removeComments <$> readFile path


getParamID :: Params -> ImageName -> Maybe FOS
getParamID params imgName = do
   let checkedPrefixes = map (checkPrefix imgName) $ M.keys params
   let foundPrefix = foldl mplus Nothing checkedPrefixes
   foundPrefix >>= flip M.lookup params >>= (\(Param _ _ _ fos) -> return fos)


checkPrefix :: String -> String -> Maybe String
checkPrefix filePath prefix = if isPrefixOf prefix filePath
   then Just prefix
   else Nothing

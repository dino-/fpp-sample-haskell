module PWS.HPC.EnkfConus.Grads.Region
   ( Regions, Region (..)
   , loadRegions
   , getRegionID
   )
   where

import qualified Data.Map as M
import Safe ( readNote )

import PWS.HPC.EnkfConus.Util ( removeComments )


type Regions = M.Map String Region

data Region = Region DbId GradsArgs GradsArgs deriving (Read, Show)

type DbId = Int

type GradsArgs = String


loadRegions :: FilePath -> IO Regions
loadRegions path = M.fromList . readNote ("ERROR parsing region file " ++ path)
   . removeComments <$> readFile path


getRegionID :: Regions -> String -> Maybe DbId
getRegionID regions region = do
   (Region dbid _ _) <- M.lookup region regions
   return dbid

module PWS.HPC.EnkfConus.WorkingDir
   ( makeIntervalDir
   , makeImagesDir
   , makeFilePostDir
   )
   where

import Control.Monad ( when )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist
   , getCurrentDirectory, setCurrentDirectory )
import System.FilePath ( (</>), takeFileName )
import System.Posix.Files ( accessModes, setFileMode )
import System.Process ( system )
import Text.Printf ( printf )

import PWS.HPC.EnkfConus.Config
   ( Config ( altDir, fosDestPrefix, fosDestPath, postWorkingDir, rsrcPath )
   , intervalPath
   , postWorkingPath
   )
import PWS.HPC.EnkfConus.Util ( createSymlinkIfMissing )


makeIntervalDir :: Config -> IO ()
makeIntervalDir conf = do
   -- First, we need to make sure the entire run working dir exists
   -- This could be the first time this run is being worked on,
   -- so just always try to do it.
   let postWorkingPath' = postWorkingPath conf
   createDirectoryIfMissing True postWorkingPath'

   -- Next, we need the working directory for this forecast hour
   let intervalPath' = intervalPath conf
   createDirectoryIfMissing True intervalPath'

   -- We also need to make sure the alt run working dir exists
   -- BUT we need to be careful if the altOutputDir is not there
   altDirExists <- doesDirectoryExist $ altDir conf
   when altDirExists $ do
      let postWorkingPathAlt = (altDir conf) </> (postWorkingDir conf)
      createDirectoryIfMissing True postWorkingPathAlt

      -- And symlink the forecast hour (interval) path there
      savedDir <- getCurrentDirectory
      setCurrentDirectory postWorkingPathAlt
      createSymlinkIfMissing intervalPath' (takeFileName intervalPath')
      setCurrentDirectory savedDir


makeImagesDir :: Config -> IO ()
makeImagesDir conf = do
   let intervalPath' = intervalPath conf

   -- We need the temp images directory for creating the graphics
   createDirectoryIfMissing True $ intervalPath' </> "images"

   {- FIXME We need this later when we write the code to be more protective of bad images!

   -- We need the images directory for the graphics that completed successfully
   createDirectoryIfMissing True $ intervalPath' </> "images_final"
   -}

   {- FIXME Revisit this later
   -- A directory for links to all completed images together
   let imagesPath = (postWorkingPath conf) </> "images"
   createDirectoryIfMissing True imagesPath

   -- And symlink the images path into the alt run working dir
   let postWorkingPathAlt = (altDir conf) </> (postWorkingDir conf)
   savedDir <- getCurrentDirectory
   setCurrentDirectory postWorkingPathAlt
   createSymlinkIfMissing imagesPath (takeFileName imagesPath)
   setCurrentDirectory savedDir
   -}


   -- Link the individual grads_tools files

   -- Set the intervalPath as the current working directory
   setCurrentDirectory intervalPath'

   -- Symlink files grads needs into this directory
   _ <- system $ printf "ln -s %s/grads_tools/* ." (rsrcPath conf)


   return ()


makeFilePostDir :: Config -> IO ()
makeFilePostDir conf = do
   let fosDestPath' = "/" ++ (fosDestPrefix conf) </> (fosDestPath conf)

   createDirectoryIfMissing True fosDestPath'
   setFileMode fosDestPath' accessModes  -- accessModes is 777

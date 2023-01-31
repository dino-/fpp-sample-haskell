module PWS.HPC.EnkfConus.Util
   ( exit
   , removeComments
   , createLinkIfMissing
   , createSymlinkIfMissing
   )
   where

import Control.Monad ( unless )
import Data.List ( isPrefixOf )
import System.Directory ( doesDirectoryExist, doesFileExist )
import System.Exit ( exitFailure, exitSuccess )
import System.Posix.Files ( createLink, createSymbolicLink )

import PWS.HPC.EnkfConus.Log


{- Turn an error message and a Maybe a into an action in our monad
   that may fail
-}
{-
maybeThrow :: String -> Maybe a -> FPP a
maybeThrow msg = maybe (throwError msg) return
-}


{- Display any final error output and make sure a sensible exit
   code is returned to the system
-}
exit :: Either String () -> IO ()
exit (Left emsg) = emergencyM lname emsg >> exitFailure
exit (Right ())  = exitSuccess


{- Remove Haskell-style -- comments from a String
   This is intended to be used prior to trying to Read a String
   into some data type.
-}
removeComments :: String -> String
removeComments = unlines . map removeComment . lines where
   removeComment = unwords . (takeWhile (not . isPrefixOf "--")) . words


createLinkIfMissing :: FilePath -> FilePath -> IO ()
createLinkIfMissing src link = do
   fileExists <- doesFileExist link
   dirExists <- doesDirectoryExist link

   unless (fileExists || dirExists) $
      createLink src link


createSymlinkIfMissing :: FilePath -> FilePath -> IO ()
createSymlinkIfMissing src link = do
   fileExists <- doesFileExist link
   dirExists <- doesDirectoryExist link

   unless (fileExists || dirExists) $
      createSymbolicLink src link

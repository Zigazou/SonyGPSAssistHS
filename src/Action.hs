{- |
Module      : Action
Description : Actions are functions which immediately exit with a message when
              there is an error.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Action
( actionIO
, action
, sonyDirs
, getSonyMounts
, httpGetString
, httpGetBin
, verifyMD5
, writeGPS
) where

import Network.Curl ( URLString, curlGetString, curlGetString_
                    , CurlCode(CurlOK)
                    )
import System.Directory ( doesDirectoryExist, getPermissions, writable
                        , createDirectoryIfMissing
                        )
import System.FilePath.Posix ((</>))
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy as B
import Control.Monad (filterM, liftM)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)

import Constant (sonyBaseDir, sonyGPSDir, sonyGPSName)
import MTab (getMTab, fsVfstype, fsFile)

{- |
An action is a function which stops the program immediately in case of an
error. Running it prints a message and "OK" if everything went well. Otherwise
it prints an error message and exit.

The function must return an `Either String a` value. Left value is the error
message.

`actionIO` is for functions which works in the IO monad.
-}
actionIO :: String -> IO (Either String a) -> IO a
actionIO msg todo = do
    putStr msg >> putStr "... " >> hFlush stdout
    result <- todo
    case result of
         Left msgError -> putStrLn msgError >> exitFailure
         Right value   -> putStrLn "OK" >> return value

{- |
An action is a function which stops the program immediately in case of an
error. Running it prints a message and "OK" if everything went well. Otherwise
it prints an error message and exit.

The function must return an `Either String a` value. Left value is the error
message.

`action` is for pure functions.
-}
action :: String -> Either String a -> IO a
action msg result = do
    putStr msg >> putStr "... " >> hFlush stdout
    case result of
         Left msgError -> putStrLn msgError >> exitFailure
         Right value   -> putStrLn "OK" >> return value

{- |
Given a list of mount points, filter this list and keep only those which
holds a Sony directory structure and which are writable.
-}
sonyDirs :: [FilePath] -> IO [FilePath]
sonyDirs dirs = filterM isItASonyDir dirs >>= filterM isWritable
    where
        isItASonyDir dir = doesDirectoryExist $ dir </> sonyBaseDir
        isWritable = liftM writable . getPermissions

{- |
An action which returns the list of mounted points which contains Sony
directory structures which are writables.
-}
getSonyMounts :: IO (Either String [FilePath])
getSonyMounts = do
    mtab <- getMTab

    let vfats = filter ((== "vfat") . fsVfstype) mtab

    sonyMounts <- sonyDirs (fsFile <$> vfats)

    return $ if null sonyMounts
                then Left "No Sony storage device found"
                else Right sonyMounts

{- |
An action using Curl to retrieve a `String` given an URL.
-}
httpGetString :: URLString -> IO (Either String String)
httpGetString url = do
    (rc, value) <- curlGetString url []
    return $ case rc of
        CurlOK -> Right value
        code   -> Left ("Cannot download " ++ url ++ " (" ++ show code ++ ")")

{- |
An action using Curl to retrieve a `ByteString` given an URL.
-}
httpGetBin :: URLString -> IO (Either String B.ByteString)
httpGetBin url = do
    (rc, value) <- curlGetString_ url []
    return $ case rc of
        CurlOK -> Right value
        code   -> Left ("Cannot download " ++ url ++ " (" ++ show code ++ ")")

{- |
An action which calculates the MD5 of a `ByteString` and compares it to
another. The MD5 must be given in an hexadecimal string.
-}
verifyMD5 :: B.ByteString -> String -> Either String ()
verifyMD5 bdata wanted
    | calculated /= wanted = Left "Incorrect MD5"
    | otherwise = Right ()
    where calculated = show $ md5 bdata

{- |
An action which writes a `ByteString` containing GPS data in a dedicated file
residing on a device of which the mount point is given in a `FilePath`.
-}
writeGPS :: FilePath -> B.ByteString -> IO (Either String ())
writeGPS dir bdata = do
    createDirectoryIfMissing True (dir </> sonyGPSDir)
    B.writeFile (dir </> sonyGPSDir </> sonyGPSName) bdata 
    return $ Right ()

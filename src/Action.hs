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
( stepIO
, step
, sonyDirs
, getSonyMounts
, httpGetString
, httpGetBin
, verifyMD5
, writeGPS
) where

import Network.Curl ( URLString, curlGetString, curlGetString_
                    , CurlCode(CurlOK), CurlOption
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

type Step a = Either String a
type StepIO a = IO (Step a)

{- |
A step is a function which stops the program immediately in case of an
error. Running it prints a message and "OK" if everything went well. Otherwise
it prints an error message and exit.

The function must return an `Either String a` value. Left value is the error
message.

`stepIO` is for functions which works in the IO monad.
-}
stepIO :: String -> StepIO a -> IO a
stepIO msg todo = do
    putStr msg >> putStr "... " >> hFlush stdout
    result <- todo
    case result of
         Left msgError -> putStrLn msgError >> exitFailure
         Right value   -> putStrLn "OK" >> return value

{- |
A step is a function which stops the program immediately in case of an
error. Running it prints a message and "OK" if everything went well. Otherwise
it prints an error message and exit.

The function must return an `Either String a` value. Left value is the error
message.

`step` is for pure functions.
-}
step :: String -> Step a -> IO a
step msg = stepIO msg . return

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
A step which returns the list of mounted points which contains Sony
directory structures which are writables.
-}
getSonyMounts :: StepIO [FilePath]
getSonyMounts = do
    vfats <- liftM (filter ((== "vfat") . fsVfstype)) getMTab

    sonyMounts <- sonyDirs (fsFile <$> vfats)

    return $ if null sonyMounts
                then Left "No Sony storage device found"
                else Right sonyMounts

{- |
Helper function for httpGetString and httpGetBin.
-}
httpGet :: (URLString -> [CurlOption] -> IO (CurlCode, a))
        -> URLString
        -> StepIO a
httpGet get url = do
    (rc, value) <- get url []
    return $ case rc of
        CurlOK -> Right value
        code   -> Left ("Cannot download " ++ url ++ " (" ++ show code ++ ")")

{- |
A step using Curl to retrieve a `String` given an URL.
-}
httpGetString :: URLString -> StepIO String
httpGetString = httpGet curlGetString

{- |
A step using Curl to retrieve a `ByteString` given an URL.
-}
httpGetBin :: URLString -> StepIO B.ByteString
httpGetBin = httpGet curlGetString_

{- |
A step which calculates the MD5 of a `ByteString` and compares it to
another. The MD5 must be given in an hexadecimal string.
-}
verifyMD5 :: B.ByteString -> String -> Step ()
verifyMD5 bdata wanted
    | calculated /= wanted = Left "Incorrect MD5"
    | otherwise = Right ()
    where calculated = show $ md5 bdata

{- |
A step which writes a `ByteString` containing GPS data in a dedicated file
residing on a device of which the mount point is given in a `FilePath`.
-}
writeGPS :: FilePath -> B.ByteString -> StepIO ()
writeGPS dir bdata = do
    createDirectoryIfMissing True (dir </> sonyGPSDir)
    B.writeFile (dir </> sonyGPSDir </> sonyGPSName) bdata 
    return $ Right ()

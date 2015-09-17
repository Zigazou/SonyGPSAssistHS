{- |
Module      : Main
Description : Sony cameras which embed a GPS are able to use assist data to
              accelerate the positioning. This program downloads and writes
              Sony GPS assist data to SD cards under Linux.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Main where

import Control.Monad (forM_)

import Constant (gpsDataUrl, gpsMD5Url)
import Action ( getSonyMounts, httpGetBin, httpGetString, verifyMD5, writeGPS
              , stepIO, step
              )

main :: IO ()
main = do
    -- Search for any writable Sony mounted storage device. 
    sonyMounts <- stepIO "Analyze mounted filesystems" getSonyMounts

    -- Download GPS data from Sony site using Curl
    gpsData <- stepIO "Retrieve GPS data from Sony site" (httpGetBin gpsDataUrl)

    -- Download corresponding MD5 file
    gpsMD5raw <- stepIO "Retrieve MD5 for GPS data from Sony site"
                        (httpGetString gpsMD5Url)

    -- Verify the MD5 matches
    step "Check MD5" (verifyMD5 gpsData $ take 32 gpsMD5raw)

    -- For any Sony mounted storage device found, copy the GPS data
    forM_ sonyMounts $ \mDir -> stepIO ("Write GPS data on " ++ mDir)
                                       (writeGPS mDir gpsData)

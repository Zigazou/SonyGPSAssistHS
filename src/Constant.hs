{- |
Module      : Constant
Description : Constant values for the SonyGPSAssist program.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Constant where

import Network.Curl (URLString)

{- |
The URL pointing the Sony GPS data file.
-}
gpsDataUrl :: URLString
gpsDataUrl = "http://control.d-imaging.sony.co.jp/GPS/assistme.dat"

{- |
The URL pointing the MD5 corresponding to the Sony GPS data file.
-}
gpsMD5Url :: URLString
gpsMD5Url = "http://control.d-imaging.sony.co.jp/GPS/assistme.md5"

{- |
Base directory allowing to identify a Sony storage device.
-}
sonyBaseDir :: FilePath
sonyBaseDir = "PRIVATE/SONY"

{- |
GPS directory where GPS data should be stored.
-}
sonyGPSDir :: FilePath
sonyGPSDir = "PRIVATE/SONY/GPS"

{- |
GPS data file name.
-}
sonyGPSName :: FilePath
sonyGPSName = "assistme.dat"

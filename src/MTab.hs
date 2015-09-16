{- |
Module      : MTab
Description : Read and parse Unix mtab file
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module MTab
( MEntry(MEntry, fsSpec, fsFile, fsVfstype, fsMntops, fsFreq, fsPassno)
, toMEntry
, toMTab
, getMTab
) where

import Data.Maybe (catMaybes)
import Control.Monad (liftM)

{- |
An MTab is simple a list of MEntry
-}
type MTab = [MEntry]

{- |
An MEntry copy the structure of a line in an mtab file.
-}
data MEntry = MEntry
        { fsSpec :: String -- ^ block special device
        , fsFile :: String -- ^ mount point
        , fsVfstype :: String -- ^ file system type
        , fsMntops :: String -- ^ mount options
        , fsFreq :: String -- ^ dump field
        , fsPassno :: String -- ^ file system check pass number
        } deriving (Eq, Show)

{- |
Converts a `String` from an mtab file to an `MEntry`.
-}
toMEntry :: String -> Maybe MEntry
toMEntry = toMEntry' . words
    where
        toMEntry' [a, b, c, d, e, f] = Just $ MEntry a b c d e f
        toMEntry' _ = Nothing

{- |
Converts lines into an `MTab`.
-}
toMTab :: String -> MTab
toMTab = catMaybes . fmap toMEntry . lines

{- |
Reads the /etc/mtab file and returns an MTab.
-}
getMTab :: IO MTab
getMTab = liftM toMTab (readFile "/etc/mtab")
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JsonHandling
( File(..)
, Folder(..)
, openFileSystem
, writeFileSystem
)where

import GHC.Generics
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy as B
import Control.Applicative

data File = File { fileName :: String
                 , content :: String
                 } deriving (Show, Generic, Eq)

instance FromJSON File
instance ToJSON File

data Folder = Folder { folderName :: String
                     , files :: [File]
                     , folders :: [Folder]
                     } deriving (Show, Generic, Eq)

instance FromJSON Folder
instance ToJSON Folder


jsonFile :: FilePath
jsonFile = "filesystem.json"

getJSON :: FilePath -> IO B.ByteString
getJSON path = B.readFile path

openFileSystem :: FilePath -> IO (Maybe Folder)
openFileSystem path = do
    d <- (decode <$> getJSON path) :: IO (Maybe Folder)
    return d

writeFileSystem :: FilePath -> Folder -> IO()
writeFileSystem path fs = do
    B.writeFile path (encode fs)

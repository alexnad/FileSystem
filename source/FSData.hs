module FSData
(FileSystem
,fsFilePath
,getRoot
,currentPath
,getFolder
,getFile
)where

import ErrorMsg
import JsonHandling
import FileData
import Data.Maybe

type FileSystem = (FilePath, Folder, [String])

fsFilePath :: FileSystem -> FilePath
fsFilePath (x, _, _) = x

getRoot :: FileSystem -> Folder
getRoot (_, x, _) = x

currentPath :: FileSystem -> [String]
currentPath (_, _, x) = x

getFolder :: FileSystem -> String -> Maybe Folder
getFolder fs dirName = if (length matches == 1)
    then Just (head matches)
    else Nothing
    where
        matches = filter (\s -> folderName s == dirName) (folders folder)
        folder = fromJust (findPath (getRoot fs) (currentPath fs))

getFile :: FileSystem -> String -> Maybe File
getFile fs file = if (length matches == 1)
    then Just (head matches)
    else Nothing
    where
        matches = filter (\s -> fileName s == file) (files folder)
        folder = fromJust (findPath (getRoot fs) (currentPath fs))

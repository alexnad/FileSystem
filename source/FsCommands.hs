module FsCommands
(pwd
,cd
,ls
,rm
,cat
,mkdir
,touch
)where

import FSData
import JsonHandling
import FileData
import ErrorMsg
import Data.Maybe

pwd :: [String] -> String
pwd [] = "/"
--pwd ["/"] = "/"
pwd path = "/" ++ (head path) ++ (pwd (tail path))

cd :: FileSystem -> [String] -> Either FileSystem String
cd fs [".."] = Left (fsFilePath fs, getRoot fs, init (currentPath fs))
cd fs ["", ""] = Left (fsFilePath fs, getRoot fs, [])
cd fs path =
    case changed of
        Just changed -> Left (fsFilePath fs, getRoot fs, newPath)
        Nothing -> Right (cdInvalidName (pwd path))
    where
        changed = (findPath (getRoot fs) newPath)
        newPath = (currentPath fs) ++ path

ls :: FileSystem -> String
ls fs = foldr (\fname acc -> fname ++ " " ++ acc) "" (fileNames ++ folderNames)
    where
        folder = fromJust (findPath (getRoot fs) (currentPath fs))
        fileNames = (map (\s -> fileName s) (files folder))
        folderNames = (map (\s -> folderName s) (folders folder))

mkdir :: FileSystem -> [String] -> Either FileSystem String
mkdir fs [dirName] = if( (containsFile currentDir dirName) || (containsFolder  currentDir dirName))
    then Right (mkdirFileExists dirName)
    else Left (fsFilePath fs, createNode (getRoot fs) (currentPath fs ++ [dirName]), currentPath fs)
    where
        currentDir = fromJust (findPath (getRoot fs) (currentPath fs))

cat :: FileSystem -> [String] -> String
cat fs [file] = if (match == Nothing)
    then catNoSuchFile file
    else content (fromJust match)
    where
        match = getFile fs file

rm :: FileSystem -> [String] -> Bool -> Either FileSystem String
rm fs [fName] isRecursive
    | file == Nothing && folder == Nothing = Right (rmInvalidName fName)
    | folder /= Nothing && (not isRecursive) = Right (rmCannotRemoveDir fName)
    | otherwise = Left (fsFilePath fs, removeNode (getRoot fs) (currentPath fs ++ [fName]), currentPath fs)
    where
        file = getFile fs fName
        folder = getFolder fs fName

touch :: FileSystem -> [String] -> FileSystem
touch fs [fName] = if( (containsFile currentDir fName) || (containsFolder  currentDir fName))
    then fs
    else (fsFilePath fs, createFile (getRoot fs) (currentPath fs ++ [fName]), currentPath fs)
    where
        currentDir = fromJust (findPath (getRoot fs) (currentPath fs))

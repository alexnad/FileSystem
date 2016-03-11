module FileData
(appendContent
,containsFile
,containsFolder
,findPath
,removeNode
,createNode
,createFile
)where

import JsonHandling

appendContent :: File -> String -> File
appendContent file newContent = File (fileName file) newContent

containsFile :: Folder -> String -> Bool
containsFile folder file = file `elem` [fileName x | x <- (files folder)]

containsFolder :: Folder -> String -> Bool
containsFolder folder folName = folName `elem` [folderName x | x <- (folders folder)]


findPath :: Folder -> [String] -> Maybe Folder
findPath folder [] = Just folder
findPath folder path =
    if (containsFolder folder (head path))
    then
        if(length path == 1)
            then Just nextNode
            else findPath nextNode (tail path)
    else
        Nothing
    where
        nextNode = head [x | x <- (folders folder), (head path) == folderName x]

{-
filterF :: (Folder a, File a) => Folder -> (a -> Bool) -> (a -> a) -> a

filterFiles :: Folder -> (File -> Bool) -> [File]
filterFolders :: Folder -> (Folder -> Bool) -> [Folder] 

f :: (Folder a, File a) =>
-}

removeNode :: Folder -> [String] -> Folder
removeNode folder [fName] = Folder (folderName folder) newFiles newFolders
    where
        newFiles = filter (\s -> fileName s /= fName) (files folder)
        newFolders = filter (\s -> folderName s /= fName) (folders folder)

removeNode folder path = Folder (folderName folder) (files folder) (oldFolders ++ [newFolder])
    where
        oldFolders = filter (\s -> folderName s /= (head path)) (folders folder)
        toBeChanged = filter (\s -> folderName s == (head path)) (folders folder)
        newFolder = removeNode (head toBeChanged) (tail path)

createNode :: Folder -> [String] -> Folder
createNode folder [dirName] = Folder (folderName folder) (files folder) (folders folder ++ [newFolder])
    where
        newFolder = Folder dirName [] []

createNode folder path = Folder (folderName folder) (files folder) (oldFolders ++ [newFolder])
    where
        oldFolders = filter (\s -> folderName s /= (head path)) (folders folder)
        toBeChanged = filter (\s -> folderName s == (head path)) (folders folder)
        newFolder = createNode (head toBeChanged) (tail path)

createFile :: Folder -> [String] -> Folder
createFile folder [fName] = Folder (folderName folder) ((files folder) ++ [newFile]) (folders folder)
    where
        newFile = File fName ""

createFile folder path = Folder (folderName folder) (files folder) (oldFolders ++ [newFolder])
    where
        oldFolders = filter (\s -> folderName s /= (head path)) (folders folder)
        toBeChanged = filter (\s -> folderName s == (head path)) (folders folder)
        newFolder = createFile (head toBeChanged) (tail path)

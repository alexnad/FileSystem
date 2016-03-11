import System.IO
import Data.Maybe
import qualified Data.List.Split as S
import ErrorMsg
import JsonHandling
import FileData
import FSData
import FsCommands

infoMenu :: String
infoMenu = "commands:\nnew \"file_path\" - create new filesystem\nsaved \"file_path\" - open an existing filesystem\nquit - leave program"

infoFS :: String
infoFS = "TODO"

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn("Welcome to fsPro. Type 'info' to get help.") 
    menu

menu :: IO()
menu = do
    putStr("fsPro>")
    command <- getLine
    if (command == "quit")
    then putStrLn("")
    else do
        handleMenuComands(S.splitOn " " command)
        menu

emptyRoot :: Folder
emptyRoot = Folder "/" [] []

handleMenuComands :: [String] -> IO()
handleMenuComands [""] = putStr("")
handleMenuComands ["info"] = putStrLn(infoMenu)
handleMenuComands ["new"] = putStrLn((invalidUseOf "new"))
handleMenuComands ["saved"] = putStrLn(invalidUseOf "saved")
handleMenuComands ["new", path] = do
    putStrLn("You have opened " ++ path ++ ". Enter 'info' for help")
    filesystemMain (path, emptyRoot, [])

handleMenuComands ["saved", path] = do
    root <- openFileSystem path
    case root of
        Nothing -> putStrLn(invalidFileSystem path)
        Just root -> filesystemMain (path, root, [])

handleMenuComands command = putStrLn((invalidCommand (head command)))


rmError :: FileSystem -> String -> IO FileSystem
rmError fs err = do
    putStrLn(err)
    return fs

fpDoesNotExist :: FileSystem -> String -> IO FileSystem
fpDoesNotExist fs path = do
    putStrLn(cdInvalidName path)
    return fs

filesystemMain :: FileSystem -> IO()
filesystemMain fs = do
    putStr("fsMain>")
    command <- getLine
    if(command == "quit")
    then writeFileSystem (fsFilePath fs) (getRoot fs)
    else do
        f <- (handleFSCommands fs (S.splitOn " " command))
        filesystemMain f

handleFSCommands :: FileSystem -> [String] -> IO FileSystem
handleFSCommands fs [""] = return fs
handleFSCommands fs ["info"] = do
    putStrLn(infoFS)
    return fs

handleFSCommands fs ["ls"] = do
    putStrLn(ls fs)
    return fs

handleFSCommands fs ["pwd"] = do
    putStrLn(pwd (currentPath fs))
    return fs

handleFSCommands fs ["cd", path] = 
    either (\ f -> return f) (fpDoesNotExist fs) (cd fs (S.splitOn "/" path))

handleFSCommands fs ["cat", path] = do
    putStrLn(cat fs (S.splitOn "/" path))
    return fs

handleFSCommands fs ["rm", path] =
    either (\f -> return f) ((rmError) fs) (rm fs (S.splitOn "/" path) False)

handleFSCommands fs ["rm", "-r", path] =
    either (\f -> return f) ((rmError) fs) (rm fs (S.splitOn "/" path) True)

handleFSCommands fs ["mkdir", path] =
    either (\f -> return f) ((rmError) fs) (mkdir fs (S.splitOn "/" path))

handleFSCommands fs ["touch", path] = return (touch fs (S.splitOn "/" path))

handleFSCommands fs command = do
    putStrLn(invalidCommand (head command)) 
    return fs

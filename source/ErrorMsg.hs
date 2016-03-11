module ErrorMsg
(rmCannotRemoveDir
,rmInvalidName
,cdInvalidName
,catNotAFile
,catNoSuchFile
,mkdirFileExists
,invalidCommand
,invalidUseOf
,invalidFileSystem
)where

rmCannotRemoveDir :: String -> String
rmCannotRemoveDir dirName = "rm: cannot remove '" ++ dirName ++ "': Is a directory. Try rm -r for recursive deletion"

rmInvalidName :: String -> String
rmInvalidName fname = "rm: cannot remove '" ++ fname ++ "': no such directory"

cdInvalidName :: String -> String
cdInvalidName fname = "cd: no such directory: " ++ fname

catNotAFile :: String -> String
catNotAFile fname = "cat: " ++ fname ++ ": Is a directory"

catNoSuchFile :: String -> String
catNoSuchFile fname = "cat: " ++ fname ++ ": No such file !"

mkdirFileExists :: String -> String
mkdirFileExists fname = "mkdir: cannot create directory " ++ fname ++ ": File exists"

invalidCommand :: String -> String
invalidCommand command = "Command not found: '" ++ command ++ "' : try 'info' for information"

invalidUseOf :: String -> String
invalidUseOf command = command ++ ": takes one argument: name of filesystem"

invalidFileSystem :: String -> String
invalidFileSystem fsName = "'" ++ fsName ++ "'not a valid filesystem"

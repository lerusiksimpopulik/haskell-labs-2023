import System.IO
import System.Directory
import qualified Data.ByteString as S
import Control.Exception
import System.Directory

ls :: FilePath -> IO ()
ls filepath = do
    files <- listDirectory filepath
    putStrLn $ unlines files

-- task 1 Sanity Check
printFile :: String -> IO ()
printFile fileToPrint = do
    handle <- openFile fileToPrint ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- task 2 Text Check
areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText f1 f2 = do
    cont1 <- readFile f1
    cont2 <- readFile f2
    return (cont1 == cont2)

-- task 3 Dos2Unix
dos2unix :: FilePath -> IO ()
dos2unix fp = do
    cont <- readFile fp
    let ans = filter(\c -> c /= '\r') cont
    writeFile fp ans

unix2dos :: FilePath -> IO ()
unix2dos fp = do
    cont <- readFile fp
    let ans =  concatMap (\c -> if c == '\n' then "\r\n" else [c]) cont
    writeFile fp ans

-- task 4 Binary check
areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin f1 f2 = do
    cont1 <- S.readFile f1
    cont2 <- S.readFile f2
    return (cont1 == cont2)

-- task 5 No Vimmers?
fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited fp = doesFileExist (fp ++ ".sw")

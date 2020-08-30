module Command 
    ( processArgs
    ) where

import System.Environment
import System.Exit

import SaveData
import Frogmon

version :: IO ()
version = putStrLn "Frog-mon Version 0.1.0"

processArgs :: IO ()
processArgs = getArgs >>= process

process :: [String] -> IO ()
process ["-h"]        = help           >> exitSuccess
process ["-help"]     = help           >> exitSuccess
process ["-v"]        = version        >> exitSuccess
process ["--version"] = version        >> exitSuccess
process ["new"]       = newFrog        >> exitSuccess
process ["check"]     = checkFrog      >> exitSuccess
process []            = checkFrog      >> exitSuccess
process _             = unknownCommand >> exitFailure


help :: IO ()
help = putStrLn "Yeah, Nah..."

newFrog :: IO Frogmon
newFrog = createFrogmon >>= save

checkFrog :: IO Frogmon
checkFrog = getFrog >>= cycleFrogmon >>= printFrogOverview >>= save

unknownCommand :: IO ()
unknownCommand = putStrLn "Unknown command, try running `frogmon -h` for help!"

getFrog :: IO Frogmon
getFrog = do
    x <- loadSave
    case x of
        Just x  -> return x
        Nothing -> couldNotGetFrog

couldNotGetFrog :: IO Frogmon
couldNotGetFrog = do
    putStrLn "Could not load frog! Would you like to create one? [n/y]"
    ans <- getLine
    case ans of
        "y" -> newFrog
        "n" -> exitSuccess
        _   -> couldNotGetFrog

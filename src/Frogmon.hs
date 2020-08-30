module Frogmon
    ( Gender(..)
    , Frogmon(..)
    , createFrogmon
    , printFrogOverview
    , cycleFrogmon
    , punishFogmon
    , cleanForgmon
    , healFrogmon
    , playFrogmon
    , feedFrogmon
    ) where

import System.Random
import Data.Time.Clock.POSIX

data Gender = Male | Female
            deriving(Show, Read)

data Frogmon = Frogmon { name             :: String
                       , age              :: Int
                       , weight           :: Int
                       , gender           :: Gender
                       , lastCycle        :: Int
                       , hunger           :: Int
                       , happiness        :: Int
                       , hygiene          :: Int
                       , entertainment    :: Int
                       , discipline       :: Int
                       , sickness         :: Int
                       , shouldDiscipline :: Bool
                       , cyclesLived      :: Int
                       , isAlive          :: Bool
                       } deriving (Show, Read)


solidRect :: [Char]
solidRect = ['\9608']

fadedRect :: [Char]
fadedRect = ['\9617']

frogEmoji :: [Char]
frogEmoji = ['\128056']

yellEmoji :: [Char]
yellEmoji = ['\128495']

skullEmoji :: [Char]
skullEmoji = ['\128128']

sickEmoji :: [Char]
sickEmoji = ['\129440']

secondsPerCycle :: Int
secondsPerCycle = 1800

secondsPerDay :: Int
secondsPerDay = 86400

cyclesPerDay :: Int
cyclesPerDay = secondsPerDay `div` secondsPerCycle

clampZero :: Int -> Int
clampZero x = if x > (-1) then x else 0

clampTen :: Int -> Int
clampTen x = if x < (11) then x else 10

askName :: IO String
askName = do
    putStrLn "What would you like to name your frog-mon?"
    name <- getLine
    if length name > 0 then
        return name
    else
        askName

getRandomGender :: IO Gender
getRandomGender = do
    num <- randomRIO (0, 1) :: IO Int
    case num of
        0 -> return Male
        1 -> return Female

getEpochAsInt :: IO Int
getEpochAsInt = round `fmap` getPOSIXTime


createFrogmon :: IO Frogmon
createFrogmon = do
    fGender <- getRandomGender
    putStrLn $ "It's a " ++ show fGender ++ "!"
    fName <- askName
    fWeight <- randomRIO (7, 13)
    fLastCycle <- getEpochAsInt
    return $ Frogmon fName 0 fWeight fGender fLastCycle 5 5 5 5 0 0 False 0 True

cycleFrogmon :: Frogmon -> IO Frogmon
cycleFrogmon frog = do
    currentTime <- getEpochAsInt
    runCycles frog 
        $ howManyCyclesToRun (lastCycle frog) currentTime where
            runCycles :: Frogmon -> Int -> IO Frogmon
            runCycles frog 0 = return frog
            runCycles frog i = do
                if (isAlive frog) then do
                    fHunger     <- calcHunger frog
                    fHygiene    <- calcHygiene frog
                    fEnjoyment  <- calcEntertainment frog
                    fSickness   <- calcSickness frog
                    fDiscipline <- calcShouldDiscipline frog
                    fIsAlive    <- calcIsAlive frog
                    currentTime <- getEpochAsInt
                    runCycles 
                        (Frogmon
                        (name frog)
                        (calcAge frog)
                        (calcWeight frog)
                        (gender frog)
                        (currentTime)
                        (fHunger)
                        (calcHappiness frog)
                        (fHygiene)
                        (fEnjoyment)
                        (calcDiscipline frog)
                        (fSickness)
                        (fDiscipline)
                        ((cyclesLived frog) + 1)
                        (fIsAlive)
                        ) (i - 1)
                else
                    runCycles frog 0

howManyCyclesToRun :: Int -> Int -> Int
howManyCyclesToRun oldTime currentTime = (currentTime - oldTime) `div` secondsPerCycle

getBar :: Int -> String
getBar n = bar [1..10] "" n where
    bar :: [Int] -> String -> Int -> String
    bar (x:[]) s 0 = s++fadedRect
    bar (x:[]) s i = s++solidRect
    bar (x:xs) s 0 = bar xs (s++fadedRect) 0
    bar (x:xs) s i = bar xs (s++solidRect) $ i - 1

printFrogOverview :: Frogmon -> IO Frogmon
printFrogOverview frog = do
    putStrLn $ getEmojiOverview (shouldDiscipline frog) (isAlive frog) (sickness frog)
    putStrLn $ "Name      : " ++ name frog 
    putStrLn $ "Gender    : " ++ (show $ gender frog)
    putStrLn $ "Age       : " ++ (show $ age frog) ++ " days"
    putStrLn $ "Weight    : " ++ (show $ weight frog) ++ " Kg"
    putStrLn $ "Hunger    : " ++ (getBar $ hunger frog)
    putStrLn $ "Hygiene   : " ++ (getBar $ hygiene frog)
    putStrLn $ "Enjoyment : " ++ (getBar $ entertainment frog)
    putStrLn $ "Happiness : " ++ (getBar $ happiness frog)
    putStrLn $ "Discipline: " ++ (getBar $ discipline frog)
    return frog


getEmojiOverview :: Bool -> Bool -> Int -> String
getEmojiOverview _ False _ = skullEmoji
getEmojiOverview True True 0 = frogEmoji ++ yellEmoji
getEmojiOverview True True x = getSickEmojiString x ++ frogEmoji ++ yellEmoji
getEmojiOverview _ _ _= frogEmoji

getSickEmojiString :: Int -> String
getSickEmojiString 1 = sickEmoji
getSickEmojiString x = sickEmoji ++ getSickEmojiString (x - 1)


calcAge :: Frogmon -> Int
calcAge frog = (cyclesLived frog) `div` cyclesPerDay

calcWeight :: Frogmon -> Int
calcWeight frog = calc $ hunger frog where
    calc :: Int -> Int
    calc x | x < 4     = clampZero $ (weight frog) - 1
           | x > 6     = (weight frog) + 1
           | otherwise = weight frog
    

calcHunger :: Frogmon -> IO Int
calcHunger frog = do
    randNum <- randomRIO (0, 2) :: IO Int
    return $ clampZero $ (hunger frog) - randNum

calcHappiness :: Frogmon -> Int
calcHappiness frog = div ((hunger frog) 
                         +(hygiene frog)
                         +(entertainment frog))
                         3

calcHygiene :: Frogmon -> IO Int
calcHygiene frog = do
    randNum <- randomRIO (0, 2) :: IO Int
    return $ clampZero $ (hygiene frog) - randNum

calcEntertainment :: Frogmon -> IO Int
calcEntertainment frog = do
    randNum <- randomRIO (0, 2) :: IO Int
    return $ clampZero $ (entertainment frog) - randNum

calcDiscipline :: Frogmon -> Int
calcDiscipline frog = if shouldDiscipline frog then
                           clampZero $ (discipline frog) - 1
                      else
                          discipline frog

calcSickness :: Frogmon -> IO Int
calcSickness frog = if (hygiene frog) < 5
                    || (happiness  frog) < 5
                    || (weight frog) < 6
                    || (weight frog) > 18
                    then do
                        randNum <- randomRIO (0, 3) :: IO Int
                        return $ clampTen $ (sickness frog) + randNum
                    else
                        return $ sickness frog

calcShouldDiscipline :: Frogmon -> IO Bool
calcShouldDiscipline frog | shouldDiscipline frog = return True
                          | otherwise = do
                                randNum <- randomRIO (0, 15) :: IO Int
                                if randNum <= 10 then
                                    return False
                                else
                                    return True

calcIsAlive :: Frogmon -> IO Bool
calcIsAlive frog = if (sickness frog) > 0 then
        if (sickness frog) >= 10 || (weight frog) < 4 then
            return False
        else do
            let score = (sickness frog)
                      + (10 - (discipline frog)) 
                      + (10 - (happiness frog))
                      + (div (age frog) 3)
            randNum <- randomRIO (0, score)
            return $ randNum < 30 
    else return True

punishFogmon :: Frogmon -> IO Frogmon
punishFogmon frog = if (shouldDiscipline frog) then
        return $ Frogmon
        (name frog)
        (age frog)
        (weight frog)
        (gender frog)
        (lastCycle frog)
        (hunger frog)
        (happiness frog)
        (hygiene frog)
        (entertainment frog)
        (clampTen ((discipline frog) + 1))
        (sickness frog)
        (False)
        (cyclesLived frog)
        (isAlive frog)
    else
        return frog

cleanForgmon :: Frogmon -> IO Frogmon
cleanForgmon frog = return $ Frogmon
    (name frog)
    (age frog)
    (weight frog)
    (gender frog)
    (lastCycle frog)
    (hunger frog)
    (happiness frog)
    (clampTen ((hygiene frog) + 1))
    (entertainment frog)
    (discipline frog)
    (sickness frog)
    (shouldDiscipline frog)
    (cyclesLived frog)
    (isAlive frog)

healFrogmon :: Frogmon -> IO Frogmon
healFrogmon frog = return $ Frogmon
    (name frog)
    (age frog)
    (weight frog)
    (gender frog)
    (lastCycle frog)
    (hunger frog)
    (happiness frog)
    (hygiene frog)
    (entertainment frog)
    (discipline frog)
    (clampZero ((sickness frog) - 1))
    (shouldDiscipline frog)
    (cyclesLived frog)
    (isAlive frog)

playFrogmon :: Frogmon -> IO Frogmon
playFrogmon frog = return $ Frogmon
    (name frog)
    (age frog)
    (weight frog)
    (gender frog)
    (lastCycle frog)
    (hunger frog)
    (happiness frog)
    (hygiene frog)
    (clampTen ((entertainment frog) + 1))
    (discipline frog)
    (sickness frog)
    (shouldDiscipline frog)
    (cyclesLived frog)
    (isAlive frog)

feedFrogmon :: Frogmon -> IO Frogmon
feedFrogmon frog = return $ Frogmon
    (name frog)
    (age frog)
    (weight frog)
    (gender frog)
    (lastCycle frog)
    (clampTen ((hunger frog) + 1))
    (happiness frog)
    (hygiene frog)
    (entertainment frog)
    (discipline frog)
    (sickness frog)
    (shouldDiscipline frog)
    (cyclesLived frog)
    (isAlive frog)


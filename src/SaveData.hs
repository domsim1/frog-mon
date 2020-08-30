module SaveData
    ( save
    , loadSave
    ) where

import Control.Exception

fileName :: String
fileName = "save.data"

save :: (Read a, Show a) => a -> IO a
save x = do
    writeFile fileName $ show x
    return x

loadSave :: (Read a, Show a) => IO (Maybe a)
loadSave = do
    x <- safeReadFile
    case x of
        Just x  -> return $ Just $! read x
        Nothing -> return Nothing

safeReadFile :: IO (Maybe String)
safeReadFile =
    (fmap Just $ readFile fileName) `catch` handle
        where
            handle :: IOException -> IO (Maybe String)
            handle _ = return Nothing

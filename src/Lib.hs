module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do xs <- getArgs foldl (>>) (return ()) $ map putStrLn xs

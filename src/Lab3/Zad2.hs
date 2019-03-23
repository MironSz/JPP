module Lab3.Zad2 where

import System.IO
import System.Environment
import Data.Char
main :: IO ()

main = do xs <-getArgs foldl (>>) (return ()) $ map putStrLn xs

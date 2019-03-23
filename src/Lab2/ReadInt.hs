module Zad2.ReadInt where

import           Data.Char

--readInts :: String ->[Int]
rdivide :: (Char -> Bool) -> String -> [String] -> [String]
rdivide f [] acc = acc
rdivide f (x:xs) (acc:accs) =
  if f x
    then rdivide f xs ([] : (acc : accs))
    else rdivide f xs ((x : acc) : accs)

divide :: (Char -> Bool) -> String -> [String]
divide f s = rdivide f s [[]]

split s = map reverse (reverse (divide (== ' ') s))

hasOnlyInt :: String -> Bool
hasOnlyInt [] = True
hasOnlyInt (x:xs) =
  if isDigit (x)
    then hasOnlyInt xs
    else False


rtoInt ::String->Int->Int
rtoInt [] acc = acc
rtoInt (x:xs) acc  = rtoInt xs (acc*10+digitToInt(x))

toInt s = rtoInt s 0

sToInts s= map toInt (filter hasOnlyInt (split s))


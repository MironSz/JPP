module Lab3.Zad1 where
import Data.Char
import Data.Either
import Control.Monad

tokenize :: String->String->[String]
tokenize "" "" = []
tokenize l "" = [l]
tokenize "" (' ':xs) = tokenize "" xs
tokenize l (' ':xs) = l:tokenize "" xs
tokenize l (x:xs) = tokenize (x:l) xs

ereadnat :: Int->String->Either String Int

eread ::String->Either String Int
eread l@('-':xs) = negate <$> ereadnat
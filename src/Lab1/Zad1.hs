module Lab1.Zad1 where

pp :: [Int]->[[Int]]
pp [] = [[]]
pp (x:xs) = []:(map(x:)(pp xs))

head::[Int]->Int
head [] = -1
head (x:xs)= x

tail :: [Int]->[Int]
tail [] = []
tail (x:xs) = xs

--(++) :: [Int]->[Int]->[Int]
--(++) [] x = x
--(++) (x:xs) ys = x:xs Lab1.Zad1.++ ys

--ifelse:: Bool->[Int]->[Int->Int
ifelse True v1 v2 = v1
ifelse False v1 v2 = v2




--fl ::forall a.([a]->Bool)->[a]->[a]
fl f [] = []
fl f (x:xs) = ifelse y x [] : fl f xs where y = f x

--myMap f [] = []
--myMap f (x:xs) = f x : myMap f xs


onToAll:: [Int]->[[Int]]->[[Int]]
onToAll a [] = [a]
onToAll a b = a : b


appTup:: Int->(([Int],[Int])->([Int],[Int]))
appTup x  = \(y,z) -> (x:y,z)

prt ::  [Int]->[([Int],[Int])]
prt [] = [([],[])]
prt (x:xs) = ([],xs) : map (appTup x) (prt xs)

appToList:: Int->[[Int]]->[[Int]]
appToList x [] = (x:[]):[]
appToList x l = map (x:) l

flatten [] = []
flatten (x:xs) = x++flatten xs

perm :: [Int]->[[Int]]
perm [] = [[]]
perm l = flatten (map (\x -> (appToList x (perm ((filter (/=x)  l))))) l)


sito :: [Int]->[Int]
sito []= []
sito (p:xs) = p : sito (filter (\y-> y `mod` p /= 0) xs)


osilnia :: Int->Int->Int
osilnia 0 a =a
osilnia a b = osilnia (a-1) b*a
silnia::Int->Int
silnia a  = osilnia a 1

oIndexOf ::Char->String->Int-> Maybe Int
oIndexOf _ [] _ = Nothing
oIndexOf a (x:xs) b
  | x==a = Just b
  | otherwise = oIndexOf a xs (b+1)

indexOf::Char->String->Maybe Int
indexOf a s= oIndexOf a s 0

opositions::Char->String->Int->[Int]->[Int]
opositions c [] counter p= p
opositions c (x:xs) counter p = ifelse (x==c) (opositions c xs (counter+1) (counter:p)) (opositions c xs (counter+1) p)

positions c s = reverse (opositions c s 0 [])



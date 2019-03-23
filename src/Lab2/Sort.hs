module Sort where
import Zadanie1


sort ::(Ord a)=>[a]->[a]
sort l = toList (fromList l)
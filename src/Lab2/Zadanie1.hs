module Zadanie1 where
import Prelude hiding(Either(..), pure)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

toList :: Tree a -> [a]
toList t = rtoList t [] where
  rtoList Empty l = l
  rtoList (Node a b c) l = rtoList b (a : rtoList c l)

instance Show a => Show (Tree a) where
  show Empty = "-"
  show (Node r l1 l2) =  a:(b++c) where
    a=head (show r)
    b="(" ++ show l1 ++ ")"
    c="[" ++ show l2 ++ "]"
--  show t = (rshow t []) where
--    rshow::Tree a -> [Char]->[Char]
--    rshow Empty acc = '-':acc
--    rshow (Node a b c) acc = head (show a) : (rshow b (rshow c acc))

instance Eq a => Eq (Tree a) where
  (==) Empty (Node _ _ _) = False
  (==) (Node _ _ _) Empty  = False
  (==) Empty Empty = True
  (==) (Node a b c) (Node d e f) = (a==d) && (b==e) && (c==f)
  (/=)  a b = not (a==b)


instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a b c )= Node (f a) (fmap f b) (fmap f c)

ifelse True v1 v2 = v1
ifelse False v1 v2 = v2

insert :: (Ord a)=> a->Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node r b c) = ifelse (a<r) (Node r (insert a b) c) (Node r b (insert a c))

insertr :: (Ord a)=> Tree a->a -> Tree a
insertr a b = insert b a

contains ::(Ord a) => a ->Tree a -> Bool
contains a Empty = False
contains a (Node r b c) = a==r || ifelse (a<r) (contains a b) (contains a c)


fromList :: (Ord a) => [a]->Tree a
fromList (x:xs) = foldl insertr Empty (x:xs)


data Either a b = Left a | Right b
instance Functor (Either e) where
  fmap f (Left e) = Left e
  fmap f (Right b) = Right (f b)

reverseRight :: Either a [b] -> Either a [b]
reverseRight e  = fmap reverse e

class Functor f=> Pointed f where
  pure :: a -> f a

instance Pointed [] where
  pure x = [x]

instance Pointed (Maybe ) where
  pure x = Just x

instance Pointed Tree  where
  pure x = Node x Empty Empty



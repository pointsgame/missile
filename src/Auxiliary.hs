module Auxiliary where

import Data.Tree
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
        where f = reverse . dropWhile isSpace

removeNearSame :: Eq a => [a] -> [a]
removeNearSame [] = []
removeNearSame [a] = [a]
removeNearSame (h:t) = if h == head t
                       then removeNearSame t
                       else h : removeNearSame t

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count p (x:xs) | p x       = 1 + count p xs
               | otherwise = count p xs

leftShift1 :: [a] -> [a]
leftShift1 list = tail list ++ [head list]

replaceSingle :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceSingle _ _ [] = error "replaceSingle: not found."
replaceSingle f g (h:t) | f h = g h : t
                        | otherwise = h : replaceSingle f g t

padRight :: a -> Int -> [a] -> [a]
padRight c n s = if length s >= n
                 then s
                 else s ++ replicate (n - length s) c

toTreeInv :: [a] -> Tree a
toTreeInv [] = error "toTreeInv: empty list."
toTreeInv (h:t) = toTreeInv' t (Node h [])
    where toTreeInv' [] tree = tree
          toTreeInv' (h':t') tree = toTreeInv' t' (Node h' [tree])
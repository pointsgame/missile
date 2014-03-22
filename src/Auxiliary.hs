module Auxiliary where

import Data.Tree
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

removeNearSame :: Eq a => [a] -> [a]
removeNearSame [] = []
removeNearSame [a] = [a]
removeNearSame (h : t) | h == head t = removeNearSame t
                       | otherwise   = h : removeNearSame t

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count p (x : xs) | p x       = 1 + count p xs
                 | otherwise = count p xs

leftShift1 :: [a] -> [a]
leftShift1 list = tail list ++ [head list]

replaceSingle :: (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceSingle _ _ [] = error "replaceSingle: not found."
replaceSingle f g (h : t) | f h       = g h : t
                          | otherwise = h : replaceSingle f g t

padRight :: a -> Int -> [a] -> [a]
padRight c n s | length s >= n = s
               | otherwise     = s ++ replicate (n - length s) c

toTreeInv :: [a] -> Tree a
toTreeInv [] = error "toTreeInv: empty list."
toTreeInv (h : t) = foldl (\tree h' -> Node h' [tree]) (Node h []) t

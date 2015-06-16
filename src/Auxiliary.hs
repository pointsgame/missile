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

fst' :: (a1, a2, a3) -> a1
fst' (a, _, _) = a
snd' :: (a1, a2, a3) -> a2
snd' (_, a, _) = a
thd' :: (a1, a2, a3) -> a3
thd' (_, _, a) = a

fst'' :: (a1, a2, a3, a4) -> a1
fst'' (a, _, _, _) = a
snd'' :: (a1, a2, a3, a4) -> a2
snd'' (_, a, _, _) = a
thd'' :: (a1, a2, a3, a4) -> a3
thd'' (_, _, a, _) = a
fth'' :: (a1, a2, a3, a4) -> a4
fth'' (_, _, _, a) = a

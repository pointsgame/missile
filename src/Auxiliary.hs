module Auxiliary ( trim
                 , leftShift1
                 , padRight
                 , toTreeInv
                 ) where

import Data.Tree
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

leftShift1 :: [a] -> [a]
leftShift1 list = tail list ++ [head list]

padRight :: a -> Int -> [a] -> [a]
padRight c n s | length s >= n = s
               | otherwise     = s ++ replicate (n - length s) c

toTreeInv :: [a] -> Tree a
toTreeInv [] = error "toTreeInv: empty list."
toTreeInv (h : t) = foldl (\tree h' -> Node h' [tree]) (Node h []) t

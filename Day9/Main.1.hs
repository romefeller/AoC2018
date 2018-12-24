{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices, unfoldr, (\\))
import Data.Time.Calendar
import Data.Bifunctor

insertSeq :: Int -> a -> [a] -> [a]
insertSeq n e xs = take n xs ++ [e] ++ drop n xs

seq1 :: Int -> [(Int,[Int],[Int])] 
seq1 n = unfoldr (\(i,s,h) -> Just ((i,s,h),(i+1,newS (i+1) s,(rem (i+1) s)++h))) (0,[0],[])
    where 
        newS :: Int -> [Int] -> [Int]
        newS 0 (0:[]) = [0,1]
        newS 1 (0:1:[]) = [0,2,1]
        newS m xs 
            | mod m 23 /= 0 =  insertSeq (ix 23 m) m xs
            | otherwise = xs \\ [xs !! (ix 23 m)]
        rem m xs
            | mod m 23 == 0 = [xs !! (ix 23 m)]
            | otherwise = []

exec :: Int -> (Int,[Int],[Int])
exec n = last $ take (n+1) $ seq1 n

whichIx :: Int -> Int -> Int -> Int
whichIx x s n 
    | s > n = -999
    | s == 0 && n < 248 = 0
    | (w == ix x n) && (mod n x == 0) = -1
    | s == n = ix x s 
    | otherwise = go n + w
    where 
        w = whichIx x s (n-1)
        z = if ix x n > w then 0 else 1
        go n = if mod n x == 0 then -1 else z

ix :: Int -> Int -> Int
ix _ 0 = 0
ix _ 1 = 1
ix _ 2 = 1
ix x n 
    | mod n x /= 0 = mod (2 + (ix x $ n-1)) (n + (mod n 2) - (div n x))   
    | otherwise = mod ((ix x $ n-1) - 7) (n - (div (n-1) x))

cyclic = let x = 0 : y 
             y = 1 : x
         in x

main :: IO ()
main = do 
    strs <- readFile "test"
    (a:b:[]) <- return $ map read (words strs) :: IO [Int]
    print $ (a,b)
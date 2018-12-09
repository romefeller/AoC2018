{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices)
import Data.Time.Calendar
import Data.Bifunctor

data RoseTree = RoseTree {
    header :: (Int,Int),
    children :: [RoseTree],
    metadata :: [Int]
} deriving Show

buildRT :: [Int] -> RoseTree
buildRT (x:y:xs) 
    | (fst $ header $ buildRT xs) /= 0  = RoseTree (x,y) (rt:[]) m
    | otherwise = RoseTree (x,y) (s:[RoseTree (a,b) [] n]) m
        where 
            (a:b:_) = take 2 xs
            n = take y $ drop 2 xs
            rt = buildRT next
            s = buildRT $ drop (2+y) next
            m = take y (reverse xs)
            next = reverse $ drop y (reverse xs)
buildRT _ = RoseTree (-1,-1) [] []

exec :: [Int] -> Int
exec (0:y:xs) = sum (take y xs) + exec (drop y xs)
exec (x:y:xs) = sum m + exec next
    where 
        m = take y (reverse xs)
        next = reverse $ drop y (reverse xs)
exec _ = 0

sumRT :: RoseTree -> Int
sumRT (RoseTree _ cs m) = sum m + sum (map sumRT cs) 

main :: IO ()
main = do 
    strs <- readFile "input"
    nums <- return $ map read (words strs) :: IO [Int]
    print $ exec nums
{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Data.Foldable (toList)
import Data.Sequence 
import Prelude hiding (take,drop,null,length,filter)
import Control.Monad.State


data RoseTree = RoseTree {
    childs ::   Int,
    meta   :: Int,
    children ::  Seq RoseTree,
    metadata :: Seq Int
} deriving Show

getFrom :: Monad m => Int -> StateT (Seq Int) m (Seq Int)
getFrom x = StateT $ \s -> return (take x s,drop x s)

hasNext :: Monad m => StateT (Seq Int) m Bool
hasNext = StateT $ \s -> return (null s,s)

getState :: Monad m => StateT (Seq Int) m (Seq Int)
getState = StateT $ \s -> return (s,s)

build :: Monad m => StateT (Seq Int) m RoseTree
build = do 
    cm <- getFrom 2
    x <- return $ cm `index` 0
    y <- return $ cm `index` 1
    if (x == 0) then do
        meta <- getFrom y
        return $ RoseTree x y empty meta
    else do
        rt <- mapM (const build) [1..x]  
        meta1 <- getFrom y
        return $ RoseTree x y (fromList rt) meta1

--- Pt 2 ---

index1 xs i  
    | i > length xs = RoseTree 0 0 empty empty
    | otherwise = xs `index` (i-1)

sumChild :: RoseTree -> Int
sumChild rt 
    | (length $ children rt) == 0 = sum $ metadata rt
    | otherwise = sum $ map sumChild $ map ((children rt) `index1`) $ toList (metadata rt)
        
----

sumRT :: RoseTree -> Int 
sumRT rt = 
    if null (children rt) then (sum $ metadata rt)
    else (sum $ metadata rt) + sum (fmap sumRT (children rt)) 

main :: IO ()
main = do 
    strs <- readFile "input"
    nums <- return $ fromList $ map read (words strs) :: IO (Seq Int)
    ev <- evalStateT build nums 
    print $ sumChild ev
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Sequence
import Prelude hiding (filter,length)
import Data.Foldable (toList)
import Control.Parallel (par,pseq)
import Data.Time
import Control.Concurrent
import Control.Concurrent.MVar

data Level = Rocky | Wet | Narrow deriving (Show,Enum,Read,Eq)

-- 5913
depth :: Int
depth = 5913

-- 8,701
target :: (Int,Int)
target = (8,701)

geoIndex :: ((Int,Int) -> Int) -> (Int,Int) -> Int 
geoIndex f (x,y)  
    | x == a && y == b = 0
    | x == 0 && y == 0 = 0 
    | y == 0 = mod (x*16807 + depth) 20183
    | x == 0 = mod (y*48271 + depth) 20183
    | otherwise = mod (f (x,y-1) * f (x-1,y)) 20183
    where 
        (a,b) = target

ff :: Seq (Int,Int) -> (Int,Int) -> Int
ff allp p = case lookup p (toList flist) of 
    Just x -> x
    Nothing -> error $ show p
    where 
        flist = fmap (\k -> (k,(geoIndex (ff allp) k)) ) allp
        
check :: Int -> Level
check x
    | z == 0 = Rocky
    | z == 1 = Wet
    | z == 2 = Narrow
    where 
        z = mod x 3

count :: Level -> Seq Int -> Int
count l = length . filter (== l) . fmap check 

allPoints :: Seq (Int,Int)
allPoints = fromList [(x,y)|x<-[0..tx],y<-[0..ty]]
    where 
        (tx,ty) = target

giThread :: MVar (Seq (Int,Int)) -> MVar (Seq Int) -> IO ()
giThread mvps mvarint = do 
    allp <- takeMVar mvps
    let res = fmap (geoIndex (ff allp)) allp
    pseq res (return ())
    putMVar mvarint res

allPointsT :: MVar (Seq (Int,Int)) -> IO ()
allPointsT mvar = do 
    pseq l (return ())
    putMVar mvar l
    where
        l = fromList [(x,y)|x<-[0..fst target],y<-[0..snd target]]

main :: IO ()
main = do 
    t0 <- getCurrentTime
    ls <- newEmptyMVar
    res <- newEmptyMVar
    forkIO (allPointsT ls)
    forkIO (giThread ls res)
    result <- takeMVar res
    w <- return $ count Wet result
    n <- return $ count Narrow result
    t1 <- getCurrentTime
    putStrLn ("sum: " ++ show (w+2*n))
    putStrLn ("time: " ++ show (abs(diffUTCTime t0 t1)) ++ " seconds")

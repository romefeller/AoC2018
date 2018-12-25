{-#LANGUAGE BangPatterns#-}
module Power where


import Data.HashMap (insert,Map,empty,lookup,(!),fromList)
import Data.List ((\\),unfoldr)
import Prelude hiding (lookup)

import qualified Data.Sequence as SQ

gen33 :: (Integer, Integer) -> [(Integer, Integer)]
gen33 (1,y) = [(a,b) | a<-[1,1+1,1+2], b<-[y,y+1,y+2]]
gen33 (300,y) = [(a,b) | a<-[300,300-1,300-2], b<-[y,y-1,y-2]]
gen33 (x,1) = [(a,b) | a<-[x,x+1,x+2], b<-[1,1+1,1+2]]
gen33 (x,300) = [(a,b) | a<-[x,x-1,x-2], b<-[300,300-1,300-2]]
gen33 (x,y) = [(a,b) | a<-[x-1,x,x+1], b<-[y-1,y,y+1]]

hundreds :: Integer -> Integer
hundreds x 
    | x < 1000 = div x 100
    | otherwise = hundreds $ mod x (10^(l-1))
    where 
        l = length $ show x

power :: Integer -> (Integer, Integer) -> Integer
power sn (x,y) = (hundreds $ (((x+10)*y) + sn)*(x+10)) - 5

allPowersSum :: [(Integer, Integer)] -> Integer -> [Integer]
allPowersSum allp serial = map (\p -> sum $ map (power serial) (gen33 p)) allp

allPoints :: [(Integer,Integer)]
allPoints = [(x,y) | x <-[1..300],y <-[1..300]]

highest :: Integer -> ((Integer,Integer),Integer)
highest serial 
    | x == 1 || x == 300 || y == 1 || y==300 = ((x,y),pow)
    | otherwise = ((x-1,y-1),pow)
    where
        (((x,y),pow):[]) = filter (\(_,p) -> p == maxPow) $ zip allPoints allPow
        allPow = allPowersSum allPoints serial
        maxPow = maximum $ allPow

-- PART 2 --

allPowers :: Integer -> SQ.Seq ((Integer,Integer),Integer)
allPowers serial = fmap (\p -> (p,power serial p)) $ SQ.fromList allPoints

insertPoints :: SQ.Seq ((Integer,Integer),Integer) -> Map (Integer,Integer) Integer
insertPoints = foldl (\m (k,v) -> insert k v m) empty

getMap = insertPoints $ allPowers 7400

summedTable = insertPoints $ fmap (accumP getMap) $ SQ.fromList allPoints

accumP :: Map (Integer,Integer) Integer -> (Integer,Integer) -> ((Integer,Integer),Integer)
accumP hs (x,y) = ((x,y),sum [hs ! (a,b) | a<-[1..x], b<-[1..y]])

window :: Map (Integer,Integer) Integer -> Integer -> (Integer,Integer) -> ((Integer,Integer),Integer)
window hs n (x,y) = ((x,y), d + a - b - c)
    where 
        d = hs ! (x+n-1,y+n-1)
        a = hs ! (x,y)
        b = hs ! (x-n+1,y)
        c = hs ! (x,y-n+1)
        
-- 233,187,13
maxWin n = filter (\((_,_),v) -> v == maxp) m
        where 
            m = map (window summedTable n) $ [(a,b)|a<-[n+1..300-n],b<-[n+1..300-n]]
            maxp = foldl (\x (_,a)-> max x a) (snd $ head m) (tail m)
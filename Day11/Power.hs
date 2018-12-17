module Power where


import Data.HashMap (insert,Map,empty,lookup)
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

genx :: Integer -> (Integer, Integer) -> SQ.Seq (Integer, Integer)
genx s (x,y) 
    | (300-x) < s-1 || (300-y) < s-1 = 
        (SQ.fromList interiorPts300) SQ.>< SQ.fromList ([(a,b) | a <-[300-(s-1) .. 300], b<-[300-(s-1) .. 300]] \\ interiorPts300)
    | otherwise = SQ.fromList interiorPts
    where
        interiorPts300 = [(a,b) | a <-[x .. 300], b<-[y .. 300]]
        interiorPts = [(a,b) | a <-[x .. x+(s-1)], b<-[y .. y+(s-1)]]
        
sumPowers :: Integer -> (Integer, Integer) -> Map (Integer, Integer) Integer -> Integer 
sumPowers n p hm = foldl (\s pg -> s + ((\(Just x) -> x) $ lookup pg hm)) 0 (genx n p)

-- writeFile "5" $  fosldl (\s e -> show e ++ "\n") "" f 
--f :: Int -> [[Integer]]
--f n = take 300 $ unfoldr (\ms -> Just $ (take 300 ms, drop 300 ms)) (map snd $ allPowers 7400) 

exec :: Integer -> Integer -> (Integer, Integer) -> Integer
exec serial n p = sumPowers n p (insertPoints $ allPowers serial)

--  maximum $ map (exec 7400 10) allPoints
{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Data.Sequence
import qualified Data.List as L
import Data.Foldable (toList)
import Prelude hiding (length,take,drop,replicate)

insertAt :: Int -> a -> Seq a -> Seq a
insertAt n e xs = take n xs >< fromList [e] >< drop n xs

deleteAt :: Int -> Seq a -> Seq a
deleteAt n xs = take n xs >< drop (n+1) xs

sconcat :: Seq (Seq a) -> Seq a 
sconcat = foldl (><) empty

listRem :: Int -> Int
listRem n = head $ ( toList $ fst $ marbles $ n-1) L.\\ (toList $ fst $ marbles n)

turns :: Int -> Int -> Seq Int
turns h n = fromList [0] >< (sconcat $ replicate (div h n) $ fromList [1..n])

scores h n = Prelude.zip3 (map (\i -> (turns h n) `index` i ) st)  st (fmap listRem st) 
    where 
        st = [23*x | x <- [1..(div h 23)]]

sumScores h n p = sum $ map (\(a,b,c) -> b+c) $ Prelude.filter (\(a,b,c) -> a == p) $ scores h n

marbles :: Int -> (Seq Int,Int)
marbles 0 = (fromList [0],1)
marbles 1 = (fromList [0,1],1)
marbles 2 = (fromList [0,2,1],1)
marbles k 
    | mod k 23 /= 0 = (insertAt nextIx k (fst prior),nextIx)
    | mod k 23 == 0 = (deleteAt delIx (fst prior),delIx)
    where 
        delIx = mod (snd prior-7) (n + (mod n 2))
        prior = marbles (k-1)
        nextIx = mod (snd prior + 2) (n+(mod n 2))
        n = length $ fst prior
        
main :: IO ()
main = do 
    strs <- readFile "test"
    (a:b:[]) <- return $ map read (words strs) :: IO [Int]
    print $ (a,b)
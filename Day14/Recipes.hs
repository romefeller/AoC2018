{-# OPTIONS_GHC -O -fglasgow-exts -ddump-simpl-stats #-}
{-# LANGUAGE BangPatterns #-}
module Recipes where

import Import
import Prelude hiding (take,drop,length,filter) 

intz :: Char -> Int 
intz '0' = 0
intz '1' = 1
intz '2' = 2
intz '3' = 3
intz '4' = 4
intz '5' = 5
intz '6' = 6
intz '7' = 7
intz '8' = 8
intz '9' = 9

data Tuple = Tuple !Int !Int !(Seq Char) !Int

recipes :: Int -> Seq Char -> Seq (Seq Char)
recipes n !seed = unfoldr func (Tuple 0 1 seed n)
    where
        func (Tuple e1 e2 ss n)
            | n <= 0 = Nothing
            | otherwise = Just (next,Tuple (ix next e1) (ix next e2) next (n-1))
            where 
                next = ss >< (fromList $! show terms)
                terms = (intz $! ss `index` e1) + (intz $! ss `index` e2)
                ix sss x = mod ((intz $ sss `index` x) +x+1) (length next)


-- UGLY OPTIMIZATIONS --
pt2' :: Int -> Seq Char -> Seq Char -> Int
pt2' !x !s !xs = case viewl xs of 
    EmptyL -> -99
    (a :< as) -> go a as
    where 
        go a !as
            | '1' == a    = go1 s as
            | otherwise = pt2' (x+1) s as 
            where
                go1 s as = case viewl as of 
                    EmptyL -> -98 
                    (b :< bs) -> 
                        if b == '1' then 
                            if s == take 4 bs then 
                                (error $ show $ (x,take 10 as)) 
                            else 
                                pt2' (x+1) s as
                        else
                            pt2' (x+1) s as

pt2 :: Seq Char -> Seq Char -> Int
pt2 !s !xs = {-# SCC "pt2" #-} length . (`index` 0) . filter ((== s) . take (length s)) $! tails xs

after :: Int -> Int -> Seq Char -> Seq Char
after i j ss = take j $ drop i ss
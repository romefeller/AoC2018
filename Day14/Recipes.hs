module Recipes where

import Import
import Prelude hiding (take,drop,length,filter) 

recipes :: Integer -> Seq Char -> Seq (Seq Char)
recipes n seed = unfoldr func (0,1,seed,n)
    where
        func :: (Int,Int,Seq Char,Integer) -> Maybe (Seq Char,(Int,Int,Seq Char,Integer))
        func (e1,e2,ss,n)
            | n <= 0 = Nothing
            | otherwise = Just (next,(ix next e1,ix next e2,next,n-1))
            where 
                next = ss >< (fromList $ show $ read [ss `index` e1] + read [ss `index` e2]) :: Seq Char
                ix :: Seq Char -> Int -> Int
                ix sss x = mod ((read $ [sss `index` x]) +x+1) (length next)

pt2 :: String -> Seq Char -> Int
pt2 s = {-# SCC "pt2" #-} length . (`index` 0) . filter ((== fromList s) . take (length $ fromList s)) . tails 

after :: Int -> Int -> Seq Char -> Seq Char
after i j ss = take j $ drop i ss
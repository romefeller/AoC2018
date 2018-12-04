module Main where 

import Control.Monad.State
import Data.Functor.Identity

oper :: Num a => Char -> (a -> a)
oper '-' = (*(-1))
oper _ = id

-- PART 2 ----

accumFreq :: (Num a, Monad m) => a -> StateT [a] m ()
accumFreq n = StateT $ \(z@(y:_)) -> return ((),(n+y):z)

getFreq :: (Num a, Monad m) => StateT [a] m a
getFreq = StateT $ \ys -> return (head ys,ys)

-- first state is the repeating frequencies
-- second is the accumulating ones
freqs :: (Eq a,Num a, Monad m) => [a] -> StateT [a] m a
freqs [] = return (-1717171)
freqs (r:rs) = do
    s <- get
    accumFreq r
    x <- getFreq
    if x `elem` s then
        return x
    else 
        freqs rs

---------------

main :: IO () 
main = do 
    strNums <- readFile "input"
    numbers <- return $ map (\(x:xs) -> oper x $ read xs) $ words strNums
    putStrLn $ show $ sum numbers -- part 1
    putStrLn $ show $ evalState (freqs (concat $ repeat numbers)) [0] -- part 2
    
    
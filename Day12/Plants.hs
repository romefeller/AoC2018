{-# LANGUAGE BangPatterns #-}
module Plants where

import Data.List (unfoldr)

-- TRAINING COMONADS W/O Comonad module-- 
class Functor w => Comonad w where 
    extract :: w a -> a 
    duplicate :: w a -> w (w a)
    
    extend :: w a -> (w a -> b) -> w b
    extend wa wab = fmap wab $ duplicate wa

data Zipper a = Zipper [a] a [a] deriving Show

goLeft :: Zipper a -> Zipper a
goLeft (Zipper [] x rs) = Zipper [] x rs  
goLeft (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

goRight :: Zipper a -> Zipper a
goRight (Zipper ls x []) = Zipper ls x []
goRight (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

instance Functor Zipper where 
    fmap f (Zipper ls a rs) = Zipper (map f ls) (f a) (map f rs)

instance Comonad Zipper where 
    extract (Zipper _ a _) = a 
    duplicate z = Zipper (tail $ iterate goLeft z) z (tail $ iterate goRight z)

reduce :: Zipper Char -> Char 
reduce z@(Zipper ls x rs) 
    | elem w notesInp = '#'
    | otherwise = '.'
    where 
        w = [(extract $ goLeft  $ goLeft z)] ++ [(extract $ goLeft z)] ++ [x] ++ 
            [(extract $ goRight z)] ++ [(extract $ goRight $ goRight z)]

initz :: Zipper Char 
initz = Zipper (repeat '.') '#' ("..#.#..##......###...###" ++ (repeat '.'))

initzInp :: Zipper Char
initzInp = Zipper (repeat '.') '#' ("....##.#.#.####..#.######..##.#.########..#...##...##...##.#.#...######.###....#...##..#.#....##.##" ++ (repeat '.'))

toList :: Int -> Int -> Zipper a -> [a]
toList l r z@(Zipper ls x rs) = left l (goLeft z) ++ [x] ++ (take r rs) 
    where 
        left l z@(Zipper ls x _)
            | l == 0 = []
            | otherwise = x : left (l-1) (goLeft z) 

gens :: Int -> Zipper Char -> [Zipper Char]
gens g seed = unfoldr go (g,seed)
    where
        go !(g,s)
            | g <= 0 = Nothing
            | otherwise = Just (next,(g-1,next)) 
            where 
                next = goRight $ extend s reduce 

calculate :: Int -> Int -> String -> Int
calculate n m ps = sum $ map (uncurry (*)) $ map (\(a,b) -> (a,if b == '.' then 0 else 1)) $ zip [0-n..m] ps 

exec' :: Int -> Int -> Int -> Zipper Char -> String
exec' n m g z = toList n m $ (iterate goLeft (last $ gens g z)) !! g

exec :: Int -> Int -> Int -> Zipper Char -> Int
exec n m g z = calculate n m $ toList n m $ (iterate goLeft (last $ gens g z)) !! g

notesInp =[".#.##",".#.#.","..#.#","##.#.","##...","..###",".##..","..#..",".##.#","####.", 
           "#...#","###.#","...#.",".#..#","#.##."]
           
notes = ["...##","..#..",".#...",".#.#." ,".#.##",".##..",".####","#.#.#","#.###","##.#.", 
         "##.##","###..","###.#","####."]
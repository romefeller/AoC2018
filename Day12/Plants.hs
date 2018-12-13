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
    | elem w notes = '#'
    | otherwise = '.'
    where 
        w = [(extract $ goLeft  $ goLeft z)] ++ [(extract $ goLeft z)] ++ [x] ++ 
            [(extract $ goRight z)] ++ [(extract $ goRight $ goRight z)]

initz :: Zipper Char 
initz = Zipper (repeat '.') '#' ("..#.#..##......###...###" ++ (repeat '.'))

initzInp :: Zipper Char
initzInp = Zipper (repeat '.') '#' ("....##.#.#.####..#.######..##.#.########..#...##...##...##.#.#...######.###....#...##..#.#....##.##" ++ (repeat '.'))

toList :: Int -> Int -> Zipper a -> [a]
toList l r (Zipper ls x rs) = (take l ls) ++ [x] ++ (take r rs) 

nextGen :: Int -> [Char] -> Zipper Char 
nextGen n xs = Zipper (repeat '.' ++ (take n xs)) (head $ drop n xs) ((drop (n+1) xs) ++ repeat '.')

gens :: Int -> Int -> [Zipper Char]
gens n m = unfoldr (\b -> Just (nextGen n $ toList n m $ extend b reduce,b)) initz

calculate :: Int -> Int -> String -> Int
calculate n m ps = sum $ map (uncurry (*)) $ map (\(a,b) -> (a,if b == '.' then 0 else 1)) $ zip [n..m] ps 

notesInp =[".#.##",".#.#.","..#.#","##.#.","##...","..###",".##..","..#..",".##.#","####.", 
           "#...#","###.#","...#.",".#..#","#.##."]
           
notes = ["...##","..#..",".#...",".#.#." ,".#.##",".##..",".####","#.#.#","#.###","##.#.", 
         "##.##","###..","###.#","####."]
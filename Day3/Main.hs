{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices, null)
import Data.Time.Calendar
import GHC.Conc.Sync

data Square = Square {
    ident :: Int,
    left :: Int,
    top :: Int,
    width :: Int,
    height :: Int
} deriving Show

toSquare :: [String] -> Square
toSquare (i:l:t:w:h:_) = Square (read i) (read l) (read t) (read w) (read h)

norm :: (Int,Int) -> Double
norm (x,y) = sqrt $ fromIntegral (x^2 + y^2)

insideSquare :: (Int,Int) -> Square -> Bool
insideSquare (x,y) (Square _ l t w h) = and [x >= l,x < l+w, y >= t,y < t+h]

execute :: [Square] -> [(Int,Int)] -> Int
execute squaresDt ls = length $ filter (\x -> length x > 1) $ 
        map (filter (== True)) $ map (\p -> map (insideSquare p) squaresDt) ls

-- #1 @ 1,3: 4x4
main :: IO ()
main = do 
    let r = mkRegex "([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
    squares <- readFile "input"
    squaresDt <- return $ 
        map (toSquare . (\(Just x) -> x) . (matchRegex r) . T.unpack) $ T.splitOn "\n" $ T.pack squares
    ziipedCoord <- return $ zip (map left squaresDt) (map top squaresDt) 
    lastSquare <- return $ head $ 
        filter (\(Square i x y _ _) -> norm(x,y) == (maximum $ map norm ziipedCoord)) squaresDt
    points <- return $ [(x,y) | x <- [0 .. left lastSquare + width lastSquare], y <- [0 .. top lastSquare + height lastSquare]]
    p1 <- return $ take (div (length points) 2) points
    p2 <- return $ drop (1+div (length points) 2) points
    print $ par (execute squaresDt p1) (execute squaresDt p2 + execute squaresDt p1)
    
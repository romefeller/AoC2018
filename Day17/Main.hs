{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Control.Lens hiding ((:<),(<|),index)
import Data.Sequence
import Prelude hiding (replicate,filter)
import Data.Foldable (toList)
import Data.Bifunctor

testGrid :: Seq (Int,Int)
testGrid = fromList [(495,y)|y<-[2..7]] ><
           fromList [(x,7)|x<-[495..501]] ><
           fromList [(501,y)|y<-[3..7]] ><
           fromList [(498,y)|y<-[2..4]] ><
           fromList [(506,y)|y<-[1..2]] ><
           fromList [(498,y)|y<-[10..13]] ><
           fromList [(504,y)|y<-[10..13]] ><
           fromList [(x,13)|x<-[498..504]] 

adjustGrid :: Seq (Int,Int) -> Seq (Int,Int)
adjustGrid = fmap (\(x,y) -> (x-495,y))

minmax :: Seq (Int,Int) -> (Int,Int,Int,Int)
minmax = (\(xs,ys) -> (minimum $ toList xs,maximum $ toList xs,minimum $ toList ys,maximum $ toList ys)) . unzip . toList 

initial :: Seq (Seq Char)
initial = replicate (maxy+2) dots
    where 
        (minx,maxx,miny,maxy) = minmax (adjustGrid testGrid)
        dots = fromList ['.' | x<-[minx..maxx+2]]

viewGrid :: Seq (Seq Char) -> IO ()
viewGrid g = mapM_ putStrLn $ toList $ fmap (toList) g

-- clays (5,0) initial (adjustGrid testGrid)
clays :: (Int,Int) -> Seq (Seq Char) -> Seq (Int, Int) -> Seq (Seq Char)
clays (px,py) g ss = case viewl ss of 
    EmptyL -> (g & element py . element px .~ '+')
    (x,y) :< as -> clays (px,py) (g & element y . element x .~ '#') as

waters :: Seq (Seq Char) -> Seq (Int, Int) -> Seq (Seq Char)
waters  g ss = case viewl ss of 
    EmptyL -> g
    (x,y) :< as -> if ('.' == (g `index` y) `index` x) then
                        waters (g & element y . element x .~ '~') as
                   else
                        waters g as

floodFill :: (Int,Int) -> Seq (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
floodFill p@(x,y) cl ws
    | y <= maxy+1 && notElem (x,y+1) cl  = south
    | y <= maxy+1 && notElem (x+1,y) cl && elem (x,y+1) cl = east 
    | y <= maxy+1 && notElem (x-1,y) cl && elem (x,y+1) cl = west
    | y <= maxy+1 && elem (x-1,y) cl && elem (x+1,y) cl && elem (x,y) cl = north
    | y <= maxy+1 && elem (northMaxX-1,y-1) ws = floodFill (northMaxX-1,y-1) cl ((x,y) <| ws)
    | otherwise = ws
    where
        north = floodFill (x,y-1) cl ((x,y) <| ws) 
        south = floodFill (x,y+1) cl ((x,y) <| ws) 
        east  = floodFill (x+1,y) cl ((x,y) <| ws) 
        west  = floodFill (x-1,y) cl ((x,y) <| ws) 
        (northMinX,northMaxX) = bimap minimum maximum $ unzip $ toList $ filter (\(_,b) -> b == y-1) $ ws
        (minx,maxx,miny,maxy) = minmax (adjustGrid testGrid)

main :: IO ()
main = undefined
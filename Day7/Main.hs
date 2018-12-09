{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (sort, findIndices, maximumBy, nub, elemIndices, (\\), unfoldr)
import Data.Time.Calendar
import Data.Bifunctor

type Path = [(String, String)]

checkPrereq :: [(String, String)] -> String -> String
checkPrereq paths l = sort $ concat $ map fst $ filter (\(_,x) -> x == l) paths

allPrereqs :: Path -> Char -> [String]
allPrereqs paths last = map (\l -> checkPrereq paths [l]) ['A' .. last]

nextOn :: [String] -> String -> String
nextOn deps done = filter (\l -> notElem l done) $ map (['A' .. 'Z'] !!) $ findIndices (== "") deps 

next :: [String] -> String -> String
next paths cut = nextOn (map (\\ cut) paths) cut

-- (String -> Maybe (String, String)) ->
start :: [String] -> String -> [String]
start deps seed = unfoldr go seed 
    where 
        go s 
            | length s < 26 = Just (s ++ (take 1 $ n s),s ++ (take 1 $ n s))
            | otherwise = Nothing
        n s = (next deps s)
    

main :: IO ()
main = do 
    let r = mkRegex "Step ([A-Z]) must be finished before step ([A-Z]) can begin."
    cs <- readFile "input"
    paths <- return $ map (\(x:y:[]) -> (x,y)) $ map ( (\(Just x) -> x) . (matchRegex r) . T.unpack) $ 
        T.splitOn "\n" $ T.pack cs 
    concPaths <- return $ bimap concat concat . unzip $ paths
    allP <- return $ nub $ uncurry (++) concPaths
    print $ last $ start (allPrereqs paths 'Z')  ""   
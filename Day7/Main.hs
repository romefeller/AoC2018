{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.List (findIndex,sort, findIndices, maximumBy, nub, elemIndices, (\\), unfoldr)
import Data.Bifunctor
import Data.Char
import Control.Monad.State
import Control.Lens 

type Path = [(String, String)]

checkPrereq :: [(String, String)] -> String -> String
checkPrereq paths l = sort $ concat $ map fst $ filter (\(_,x) -> x == l) paths

allPrereqs :: Path -> Char -> [String]
allPrereqs paths lasto = map (\l -> checkPrereq paths [l]) ['A' .. lasto]

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

-- PT 2 --
data Process = Process{
    _sec :: Int,
    _workers :: String,
    _ticks :: [Int],
    _done :: String
} deriving Show

makeLenses ''Process

getS :: Monad m => StateT Process m Process
getS = StateT $ \s -> return (s,s)

setS :: Monad m => Process -> StateT Process m ()
setS p = StateT $ \s -> return ((),p)

alloc :: String -> Char -> String
alloc ss ch = 
    case findIndex (== '.') ss of
        Nothing -> ss
        Just i -> ss & element i .~ ch

unalloc :: String -> [Int] -> Int -> (String,[Int])
unalloc zs ts i = (zs & element i .~ '.',ts & element i .~ 0)

work :: [String] -> StateT Process IO Int
work deps = do 
    dd <- return $ deps
    p@(Process s w t d) <- getS
    ne <- return $ next deps d
    lift $ print (p,ne)
    if ne == "" then do
        error $ show (s,w,t,d)
        return s 
    else do 
        setS ( p & ((ticks .  traverse -~ 1) . (sec +~ 1)) )
        newp@(Process s1 _ t1 d1) <- getS
        if any (== 0) t1 then do
            ind <- return $ findIndices (== 0) t1
            mapM_ (\n -> getS >>= \sp@(Process _ ww tt dd) -> return (unalloc ww tt n) >>= \(nw,nt) -> setS( sp & ((workers .~ nw) . (ticks .~ nt) . (done <>~ [_workers sp !! n])))) ind
            return s1
        else 
            return s1
        np1 <- getS
        nee <- return $ next deps (_done np1)
        nextWorkers <- return $ nee \\ (_workers np1)
        if (any (== '.') $ _workers np1) && (any (\x -> notElem x $ _workers np1) nee) then do
            mapM_ (\n -> getS >>= \sp@(Process _ ww tt _) -> return (alloc ww n) >>= \nw -> setS ( sp & ((workers .~ nw) . (ticks .~ map ((\c -> if c /= '.' then ord c - 4 else -999)) nw ))) ) 
                nextWorkers
            np2 <- getS
            setS $ np2 & ticks .~ (map (\(x,y) -> if x <=0 then y else min x y ) $ zip (_ticks np1) (_ticks np2))
            return s1
        else
            return s1
        work deps
        
    
---

main :: IO ()
main = do 
    let r = mkRegex "Step ([A-Z]) must be finished before step ([A-Z]) can begin."
    cs <- readFile "input"
    paths <- return $ map (\(x:y:[]) -> (x,y)) $ map ( (\(Just x) -> x) . (matchRegex r) . T.unpack) $ 
        T.splitOn "\n" $ T.pack cs 
    concPaths <- return $ bimap concat concat . unzip $ paths
    allP <- return $ nub $ uncurry (++) concPaths
    print $ last $ start (allPrereqs paths 'Z')  ""  
    print $ (allPrereqs paths 'Z')
    s <- evalStateT (work (allPrereqs paths 'Z')) (Process 0 "....." [minBound,minBound,minBound,minBound,minBound] "")
    print s
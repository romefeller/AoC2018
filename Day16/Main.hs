{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.Bits
import Control.Applicative
import Control.Lens
import Data.List (findIndices,findIndex,zip4)

data Instruction b a = Instruction {
    opcode :: b,
    regA :: a,
    regB :: a,
    regC :: a
} deriving Show

type Register = [Int]

(>==<) :: Register -> Register -> Bool
(_:a:b:c:[]) >==< (_:x:y:z:[]) = b == y && c == z && a == x
_ >==< _ = False

instance Eq a => Eq (Instruction b a) where 
    (Instruction _ b c d) == (Instruction _ f g h) = b == f && c == g && d == h

instance Functor (Instruction b) where 
    fmap f (Instruction a b c d) = Instruction a (f b) (f c) (f d)

operi :: (Int -> Int -> Int) -> Register -> Instruction a Int -> Register
operi f xs (Instruction _ a b c) = xs & element c .~ ((xs !! a) `f` b) 

operr :: (Int -> Int -> Int) -> Register -> Instruction a Int -> Register
operr f xs (Instruction _ a b c) = xs & element c .~ ((xs !! a) `f` (xs !! b)) 

operrr :: (Int -> Int -> Bool) -> Register -> Instruction a Int -> Register
operrr f xs (Instruction _ a b c) = xs & element c .~ (if (xs !! a) `f` (xs !! b) then 1 else 0) 

operir :: (Int -> Int -> Bool) -> Register -> Instruction a Int -> Register
operir f xs (Instruction _ a b c) = xs & element c .~ (if a `f` (xs !! b) then 1 else 0) 

operri :: (Int -> Int -> Bool) -> Register -> Instruction a Int -> Register
operri f xs (Instruction _ a b c) = xs & element c .~ (if (xs !! a) `f` b then 1 else 0) 

seti :: Register -> Instruction a Int -> Register
seti xs (Instruction _ a b c) = xs & element c .~ a 

toFuncInt :: Int -> (Register -> Instruction a Int -> Register)
toFuncInt 7 = toFunc "gtri"


toFunc :: String -> (Register -> Instruction a Int -> Register)
toFunc "addi" = operi (+)
toFunc "addr" = operr (+)
toFunc "muli" = operi (*)
toFunc "mulr" = operr (*)
toFunc "seti" = seti
toFunc "setr" = operr const
toFunc "bani" = operi (.&.)
toFunc "banr" = operr (.&.)
toFunc "bori" = operi (.|.)
toFunc "borr" = operr (.|.)
toFunc "gtrr" = operrr (>)
toFunc "gtir" = operir (>)
toFunc "gtri" = operri (>)
toFunc "eqrr" = operrr (==)
toFunc "eqri" = operri (==)
toFunc "eqir" = operir (==)
--toFunc "error" = (\ _ _ -> [-99,-99,-99,-99])
toFunc _ = error $ "oper invalid"

toInst :: [String] -> Instruction Int Int
toInst (a:b:c:d:[]) = fmap read $ Instruction (read a) b c d
toInst _ = error "Invalid format"

names :: [String]
names = ["addi","addr","muli","mulr","bani","banr","bori","borr"
         ,"eqir","eqri","eqrr","gtir","gtrr","gtri","seti","setr"]

arrfns :: [Register -> Instruction Int Int -> Register]
arrfns = map toFunc names

applyAll :: Register -> Instruction Int Int -> Register -> Int
applyAll bf mi af = length $ filter (== af) $ arrfns <*> pure bf <*> pure mi

pt1 :: IO ()
pt1 = do 
    let r = mkRegex "Before: \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\\]"
    let s = mkRegex "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)"
    let t = mkRegex "After:  \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\\]"
    registers <- readFile "input"
    bf <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex r . T.unpack) $ T.splitOn "\n" $ T.pack registers
    mi <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex s . T.unpack) $ T.splitOn "\n" $ T.pack registers
    af <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex t . T.unpack) $ T.splitOn "\n" $ T.pack registers
    -- 573
    -- pt2 figuring out
    let ff = map read 
    (bfs,mis,afs) <- return $ (map ff bf, map toInst mi, map ff af)
    allidx <- return $ findIndices ((== 2) . length) $ map (\(b,m,a) -> filter (== a) $ arrfns <*> pure b <*> pure m) $ zip3 bfs mis afs
    let check n = filter ((== (afs !! n)) . snd) $ zip names (arrfns <*> pure (bfs !! n) <*> pure (mis !! n))
    print $ map check allidx

idxs xs 
    | length xs == 1 = head xs
    | length xs > 1 = 16
    
main :: IO ()
main = do 
    let s = mkRegex "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)"
    registers <- readFile "input2"
    mi <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex s . T.unpack) $ T.splitOn "\n" $ T.pack registers    
    print mi
    
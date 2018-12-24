{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.Bits

data Instruction a = Instruction {
    opcode :: String,
    regA :: a,
    regB :: a,
    regC :: a
} deriving Show

type Register = [Int]

instance Functor Instruction where 
    fmap f (Instruction a b c d) = Instruction a (f b) (f c) (f d)

operi :: (Int -> Int -> Int) -> Register -> Instruction Int -> Register
operi f xs (Instruction _ a b c) = xs & element c .~ ((xs !! a) `f` b) 

operr :: (Int -> Int -> Int) -> Register -> Instruction Int -> Register
operr f xs (Instruction _ a b c) = xs & element c .~ ((xs !! a) `f` (xs !! b)) 

operrr :: (Int -> Int -> Bool) -> Register -> Instruction Int -> Register
operrr f xs (Instruction _ a b c) = xs & element c .~ (if (xs !! a) `f` (xs !! b) then 1 else 0) 

operir :: (Int -> Int -> Bool) -> Register -> Instruction Int -> Register
operir f xs (Instruction _ a b c) = xs & element c .~ (if a `f` (xs !! b) then 1 else 0) 

operri :: (Int -> Int -> Bool) -> Register -> Instruction Int -> Register
operri f xs (Instruction _ a b c) = xs & element c .~ (if (xs !! a) `f` b then 1 else 0) 

seti :: Register -> Instruction Int -> Register
seti xs (Instruction _ a b c) = xs & element c .~ a 

toFunc :: String -> (Register -> Instruction Int -> Register)
toFunc "addi" = operi (+)
toFunc "addr" = operr (+)
toFunc "muli" = operi (*)
toFunc "mulr" = operr (*)
toFunc "seti" = seti
toFunc "setr" = operr const
toFunc "bani" = operi (.&.)
toFunc "banr" = operr (.&.)
toFunc "bori" = operi (.|.)
toFunc "borr" = operi (.|.)
toFunc "gtrr" = operrr (>)
toFunc "gtir" = operir (>)
toFunc "gtri" = operri (>)
toFunc "eqrr" = operrr (==)
toFunc "eqri" = operri (==)
toFunc "eqir" = operir (==)
toFunc _ = error $ "oper invalid"

updMem :: Monad m => Int -> Int -> StateT Register m ()
updMem ix va = StateT $ \s -> return ((),s & element ix .~ va) 

chgMem :: Monad m => Register -> StateT Register m ()
chgMem mm = StateT $ \s -> return ((),mm) 

getMem :: Monad m => StateT Register m Register
getMem = StateT $ \s -> return (s,s)

exec :: Int -> Int -> [Instruction Int] -> StateT Register IO Int
exec iip ip is = do 
    me <- getMem
    if ip < length is then do
        updMem iip ip
        upme <- getMem
        let opc = opcode $ is !! (upme !! iip)
        chgMem $ (toFunc opc) upme (is !! (upme !! iip))
        chme <- getMem
        if opc == "eqrr" then
            lift $ print (is !! (upme !! iip),upme,chme,ip)
        else 
            return ()
        exec iip (1 + chme !! iip) is
    else do
        mem <- getMem
        return $ head mem

main :: IO ()
main = do 
    let r = mkRegex "([a-z]+) ([0-9]+) ([0-9]+) ([0-9]+)"
    registers <- readFile "input"
    bf <- return $ map (\(a:b:c:d:_) -> fmap read $ Instruction a b c d) $ map (\(Just x) -> x) $ map (matchRegex r . T.unpack) $ 
        T.splitOn "\n" $ T.pack registers :: IO [Instruction Int]
    -- Part 2 with 1 -- 
    res <- evalStateT (exec 5 0 bf) [10780778,0,0,0,0,0]
    print res
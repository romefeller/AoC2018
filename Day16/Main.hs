{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Text.Regex
import qualified Data.Text as T
import Data.Bits
import Control.Applicative

data Instruction a = Instruction {
    opcode :: a,
    regA :: a,
    regB :: a,
    regC :: a
} deriving Show

instance Eq a => Eq (Instruction a) where 
    (Instruction _ b c d) == (Instruction _ f g h) = b == f && c == g && d == h

instance Functor Instruction where 
    fmap f (Instruction a b c d) = Instruction (f a) (f b) (f c) (f d)

(>!<) :: Instruction Int -> Int -> Int 
(>!<) ins i 
    | i == 0 = opcode ins
    | i == 1 = regA ins
    | i == 2 = regB ins
    | i == 3 = regC ins

setIndex :: Instruction Int -> Int -> Int -> Instruction Int 
setIndex (Instruction a b c d) i x
    | i == 0 = Instruction x b c d
    | i == 1 = Instruction a x c d
    | i == 2 = Instruction a b x d
    | i == 3 = Instruction a b c x

operi :: (Int -> Int -> Int) -> Instruction Int -> Instruction Int -> Instruction Int
operi f i1@(Instruction o1 ra rb rc) i2@(Instruction o2 va vb vc)  = 
    setIndex i1 vc (f (i1 >!< va) vb)

operr :: (Int -> Int -> Int) -> Instruction Int -> Instruction Int -> Instruction Int
operr f i1@(Instruction o1 ra rb rc) i2@(Instruction o2 va vb vc) = 
    setIndex i1 vc (f (i1 >!< va) (i1 >!< vb))

operir :: (Int -> Int -> Bool) -> Instruction Int -> Instruction Int -> Instruction Int
operir f i1@(Instruction o1 ra rb rc) i2@(Instruction o2 va vb vc)  = 
    setIndex i1 vc (if f va (i1 >!< vb) then 1 else 0)

operri :: (Int -> Int -> Bool) -> Instruction Int -> Instruction Int -> Instruction Int
operri f i1@(Instruction o1 ra rb rc) i2@(Instruction o2 va vb vc)  = 
    setIndex i1 vc (if f (i1 >!< va) vb then 1 else 0)

operrr :: (Int -> Int -> Bool) -> Instruction Int -> Instruction Int -> Instruction Int
operrr f i1@(Instruction o1 ra rb rc) i2@(Instruction o2 va vb vc) = 
    setIndex i1 vc (if f (i1 >!< va) (i1 >!< vb) then 1 else 0)

addi :: Instruction Int -> Instruction Int -> Instruction Int
addi = operi (+)

addr :: Instruction Int -> Instruction Int -> Instruction Int
addr = operr (+)

muli :: Instruction Int -> Instruction Int -> Instruction Int
muli = operi (*)

mulr :: Instruction Int -> Instruction Int -> Instruction Int
mulr = operr (*)

bani :: Instruction Int -> Instruction Int -> Instruction Int
bani = operi (.&.)

banr :: Instruction Int -> Instruction Int -> Instruction Int
banr = operr (.&.)

bori :: Instruction Int -> Instruction Int -> Instruction Int
bori = operi (.|.)

borr :: Instruction Int -> Instruction Int -> Instruction Int
borr = operr (.|.)

seti :: Instruction Int -> Instruction Int -> Instruction Int
seti i1@(Instruction o1 ra rb rc) (Instruction o2 va vb vc) = setIndex i1 vc va

setr :: Instruction Int -> Instruction Int -> Instruction Int
setr = operr const
    
eqir :: Instruction Int -> Instruction Int -> Instruction Int
eqir = operir (==)

eqri :: Instruction Int -> Instruction Int -> Instruction Int
eqri = operri (==)

eqrr :: Instruction Int -> Instruction Int -> Instruction Int
eqrr = operrr (==)

gtir :: Instruction Int -> Instruction Int -> Instruction Int
gtir = operir (>)

gtri :: Instruction Int -> Instruction Int -> Instruction Int
gtri = operri (>)

gtrr :: Instruction Int -> Instruction Int -> Instruction Int
gtrr = operrr (>)

toInst :: [String] -> Instruction Int
toInst (a:b:c:d:[]) = fmap read $ Instruction a b c d
toInst _ = error "Invalid format"

arrfns :: [Instruction Int -> Instruction Int -> Instruction Int]
arrfns = [addi,addr,muli,mulr,bani,banr,bori,borr,eqri,eqri,eqrr,gtir,gtrr,gtri,seti,setr]

applyAll :: Instruction Int -> Instruction Int -> Instruction Int -> Int
applyAll bf mi af = length $ filter (== af) $ arrfns <*> pure bf <*> pure mi

-- |([0-9]) ([0-9]) ([0-9]) ([0-9])|After:  \\[([0-9]), ([0-9]), ([0-9]), ([0-9])\\]
main :: IO ()
main = do 
    let r = mkRegex "Before: \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\\]"
    let s = mkRegex "([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)"
    let t = mkRegex "After:  \\[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\\]"

    registers <- readFile "input"
    --print registers
    bf <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex r . T.unpack) $ T.splitOn "\n" $ T.pack registers
    mi <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex s . T.unpack) $ T.splitOn "\n" $ T.pack registers
    af <- return $ map (\(Just x) -> x) $ filter (/= Nothing) $ map (matchRegex t . T.unpack) $ T.splitOn "\n" $ T.pack registers
    
    (bfs,mis,afs) <- return $ (map toInst bf, map toInst mi, map toInst af)
    print $ length $ filter (>= 3) $ map (\(b,m,a) -> applyAll b m a) $ zip3 bfs mis afs
module Main where 

oper :: Num a => Char -> (a -> a)
oper '-' = (*(-1))
oper _ = id

main :: IO () 
main = do 
    strNums <- readFile "input"
    numbers <- return $ map (\(x:xs) -> oper x $ read xs) $ words strNums
    putStrLn $ show $ sum numbers
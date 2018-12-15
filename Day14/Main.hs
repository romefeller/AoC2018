module Main where 

import Recipes
import Import

lastThing seq = case viewr seq of
   EmptyR -> error "no end"
   as :> a -> a

main :: IO ()
main = print $ pt2' 0 (fromList "0201") (lastThing (recipes 12000002 (fromList "37"))) 
module Main where 

import Recipes
import Import

main :: IO ()
main = print $ pt2 "110201" ((recipes 20000001 (fromList "37")) `index` 20000000)
module Main where

import TwentyFortyEight

main :: IO ()
main = standardGameDriver atStart maximiseBlanks >>= (\a -> putStrLn $ toString a)

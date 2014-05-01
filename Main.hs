module Main where

import TwentyFortyEight

main :: IO ()
main = standardGameDriver atStart randomStrategy >>= (\a -> putStrLn $ toString a)

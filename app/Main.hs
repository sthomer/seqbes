module Main where

import Lib

main :: IO ()
main = runMultiple (FileName "./data/moby-dick.txt")

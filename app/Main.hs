module Main where

import Data.Insertable

main :: IO ()
main = do
    print exampleWithDagger
    print exampleWithSword
    print exampleWithDaggerAndSword
    print exampleWithBunch
    print exampleWithDaggersAndSwords

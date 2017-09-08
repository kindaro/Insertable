module Main where

import Data.Insertable
import Data.Insertable.Example

exampleWithDagger = Dagger #> Dagger #> mempty :: Armoury
exampleWithSword = mempty <# Sword <# Sword :: Armoury
exampleWithDaggerAndSword = exampleWithDagger #> exampleWithSword
exampleWithBunch = [Dagger, Dagger] #> exampleWithSword #> exampleWithDagger
exampleWithDaggersAndSwords
    = Dagger #> Sword
    #> [Dagger, Dagger]
    #> Daggers [Dagger, Dagger] #> Swords []
    #> (mempty :: Armoury)
exampleWithDaggers = (mempty :: Daggers) #> (mempty :: Daggers)

main :: IO ()
main = do
    print exampleWithDagger
    print exampleWithSword
    print exampleWithDaggerAndSword
    print exampleWithBunch
    print exampleWithDaggersAndSwords
    print exampleWithDaggers

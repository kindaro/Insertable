{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Armoury = Armoury { daggers :: Integer, swords :: Integer } deriving Show
data Dagger = Dagger
data Sword = Sword

instance Monoid Armoury where
    mempty = Armoury 0 0
    mappend x y = Armoury { daggers = daggers x + daggers y, swords = swords x + swords y }

class Monoid bag => Insertable nut bag where
    iput :: nut -> bag -> bag
    -- ifold :: (Foldable bunch) => bunch nut -> bag

instance Insertable Dagger Armoury where
    iput x xs = xs { daggers = daggers xs + 1 }

instance Insertable Sword Armoury where
    iput x xs = xs { swords = swords xs + 1 }


exampleWithDagger = Dagger `iput` (Dagger `iput` mempty) :: Armoury

exampleWithSword = Sword `iput` (Sword `iput` mempty) :: Armoury

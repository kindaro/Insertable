{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
    (#>), iput :: nut -> bag -> bag
    (<#)       :: bag -> nut -> bag
    -- ifold :: (Foldable bunch) => bunch nut -> bag
    infixl 9 <#
    infixr 9 #>
    (<#) = flip iput
    (#>) = iput

instance Insertable Dagger Armoury where
    iput x xs = xs { daggers = daggers xs + 1 }

instance Insertable Sword Armoury where
    iput x xs = xs { swords = swords xs + 1 }


exampleWithDagger = Dagger #> Dagger #> mempty :: Armoury

exampleWithSword = mempty <# Sword <# Sword :: Armoury

instance (Monoid bag) => Insertable bag bag where
    -- It's actually always monoid, I could drop the qualifier if type system were clever.
    -- In particular, the second "bag" here is set to be Monoid in the Insertable class declaration.
    -- The type checker at present (ghc 8.0.1) can't infer that the first "bag" here is also (the
    -- same) Monoid.
    iput = mappend

exampleWithDaggerAndSword = exampleWithDagger #> exampleWithSword

instance (Foldable bunch, Insertable nut bag) => Insertable (bunch nut) bag where
    iput = flip (foldr iput)

exampleWithBunch = [Dagger, Dagger] #> exampleWithSword #> exampleWithDagger


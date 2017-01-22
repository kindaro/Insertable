{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Insertable where

data Dagger  = Dagger                                            deriving Show
data Sword   = Sword                                             deriving Show
data Daggers = Daggers [Dagger]                                  deriving Show
data Swords  = Swords [Sword]                                    deriving Show
data Armoury = Armoury { daggers :: Integer, swords :: Integer } deriving Show

class Monoid bag => Insertable nut bag where
    (#>), iput :: nut -> bag -> bag
    (<#)       :: bag -> nut -> bag
    -- ifold :: (Foldable bunch) => bunch nut -> bag
    infixl 9 <#
    infixr 9 #>
    (<#) = flip iput
    (#>) = iput
    iput = (#>)

instance (Monoid bag) => Insertable bag bag where
    -- It's actually always monoid, I could drop the qualifier if type system were clever.
    -- In particular, the second "bag" here is set to be Monoid in the Insertable class declaration.
    -- The type checker at present (ghc 8.0.1) can't infer that the first "bag" here is also (the
    -- same) Monoid.
    iput = mappend

instance (Foldable bunch, Insertable nut bag) => Insertable (bunch nut) bag where
    iput = flip (foldr iput)

instance Monoid Armoury where
    mempty = Armoury 0 0
    mappend x y = Armoury { daggers = daggers x + daggers y, swords = swords x + swords y }

instance Monoid Daggers where
    mempty = Daggers []
    mappend (Daggers x) (Daggers y) = Daggers $ x `mappend` y

instance Monoid Swords where
    mempty = Swords []
    mappend (Swords x) (Swords y) = Swords $ x `mappend` y

instance Insertable Dagger Armoury where
    iput x xs = xs { daggers = daggers xs + 1 }

instance Insertable Sword Armoury where
    iput x xs = xs { swords = swords xs + 1 }

instance Insertable Dagger Daggers where
    iput dagger (Daggers daggers) = Daggers $ dagger:daggers

instance Insertable Sword Swords where
    iput sword (Swords swords) = Swords $ sword:swords

instance Insertable Daggers Armoury where
    xs #> armoury = Armoury { daggers = sniffLength xs, swords = swords armoury }
        where sniffLength (Daggers x) = fromIntegral . length $ x

instance Insertable Swords Armoury where
    xs #> armoury = Armoury { swords = sniffLength xs, daggers = daggers armoury }
        where sniffLength (Swords x) = fromIntegral . length $ x

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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Insertable.Example
    ( Dagger(..)
    , Sword(..)
    , Daggers(..)
    , Swords(..)
    , Armoury(..)
    )
    where

import Data.Insertable

data Dagger  = Dagger                                            deriving Show
data Sword   = Sword                                             deriving Show
data Daggers = Daggers [Dagger]                                  deriving Show
data Swords  = Swords [Sword]                                    deriving Show
data Armoury = Armoury { daggers :: Integer, swords :: Integer } deriving Show

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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Main where

import Data.List
import Test.QuickCheck

import Data.Insertable

instance Arbitrary Armoury where
    arbitrary = do
        m <- choose (0,96) :: Gen Integer
        n <- choose (0,96) :: Gen Integer
        return $ Armoury m n


main :: IO ()
main = do
    quickCheck propMinBound
    quickCheck propMaxBound
    sample' (arbitrary :: Gen S) >>= foldList >>= print
    generate (arbitrary :: Gen P) >>= print
    sequence_ . fmap print =<< generate (listOf1 (listOf1 (arbitrary :: Gen S) >>= foldList'))

foldList = fmap (foldr (#>) (PArmoury mempty)) . sequence . fmap strategy . sort
foldList' = fmap (foldr (#>) (PArmoury mempty)) . sequence . fmap strategy' . sort

propMinBound :: [Armoury] -> Property
propMinBound xs =
    let z = xs #> (mempty :: Armoury) in
    not (null xs) ==> daggers z >= 0 && swords z >= 0

propMaxBound :: [Armoury] -> Property
propMaxBound xs =
    let
        z = xs #> (mempty :: Armoury)
        l = fromIntegral . length $ xs
    in
        True ==> daggers z <= (l * 96) && swords z <= (l * 96)

data S = SDagger | SSword | SDaggers | SSwords | SArmoury deriving (Enum, Eq, Ord)

data P  = PDagger Dagger
        | PSword Sword
        | PDaggers Daggers
        | PSwords Swords
        | PArmoury Armoury
        deriving (Show)

instance Arbitrary S where
    arbitrary = do
        kind <- choose (0,4) :: Gen Integer
        return $ case kind of
            0 -> SDagger
            1 -> SSword
            2 -> SDaggers
            3 -> SSwords
            4 -> SArmoury

instance Arbitrary P where
    arbitrary = do
        s <- arbitrary :: Gen S
        strategy' s

instance Arbitrary Dagger where
    arbitrary = return Dagger

instance Arbitrary Sword where
    arbitrary = return Sword

instance Arbitrary Daggers where
    arbitrary = do
        n <- choose (0,96)
        return $ Daggers (replicate n Dagger)

instance Arbitrary Swords where
    arbitrary = do
        n <- choose (0,96)
        return $ Swords (replicate n Sword)

instance Monoid P where
    mempty = PArmoury mempty
    -- Here should be mentioned those objects in the graph of insertion that loop.
    (PDaggers xs) `mappend` (PDaggers ys) = PDaggers $ xs `mappend` ys
    (PSwords xs) `mappend` (PSwords ys) = PSwords $ xs `mappend` ys
    (PArmoury xs) `mappend` (PArmoury ys) = PArmoury $ xs `mappend` ys

instance Insertable P P where
    -- Here should be mentioned all edges in the graph of insertion.
    (PDagger x) `iput` (PDaggers xs) = PDaggers $ x `iput` xs
    (PSword x) `iput` (PSwords xs) = PSwords $ x `iput` xs
    (PDagger x) `iput` (PArmoury xs) = PArmoury $ x `iput` xs
    (PSword x) `iput` (PArmoury xs) = PArmoury $ x `iput` xs
    (PDaggers x) `iput` (PArmoury xs) = PArmoury $ x `iput` xs
    (PSwords x) `iput` (PArmoury xs) = PArmoury $ x `iput` xs
    (PArmoury x) `iput` (PArmoury xs) = PArmoury $ x `iput` xs


strategy :: S -> IO P
strategy x = generate $ case x of
    SDagger -> fmap PDagger (arbitrary :: Gen Dagger)
    SSword -> fmap PSword (arbitrary :: Gen Sword)
    SDaggers -> fmap PDaggers (arbitrary :: Gen Daggers)
    SSwords -> fmap PSwords (arbitrary :: Gen Swords)
    SArmoury -> fmap PArmoury (arbitrary :: Gen Armoury)

strategy' :: S -> Gen P
strategy' x = case x of
    SDagger -> fmap PDagger (arbitrary :: Gen Dagger)
    SSword -> fmap PSword (arbitrary :: Gen Sword)
    SDaggers -> fmap PDaggers (arbitrary :: Gen Daggers)
    SSwords -> fmap PSwords (arbitrary :: Gen Swords)
    SArmoury -> fmap PArmoury (arbitrary :: Gen Armoury)


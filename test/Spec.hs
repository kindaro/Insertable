
module Main where

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


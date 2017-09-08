{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Insertable where

class Insertable nut bag where
    (#>), iput :: nut -> bag -> bag
    (<#)       :: bag -> nut -> bag
    -- ifold :: (Foldable bunch) => bunch nut -> bag
    infixl 9 <#
    infixr 9 #>
    (<#) = flip iput
    (#>) = iput
    iput = (#>)

instance (Monoid bag) => Insertable bag bag where
    iput = mappend

instance (Foldable bunch, Insertable nut bag) => Insertable (bunch nut) bag where
    iput = flip (foldr iput)

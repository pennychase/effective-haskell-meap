{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}

module Selector where

import Data.Kind

-- Chpater 6 Examples

-- Select is a simplified version of Alternative that is polymorphic over parametrized types

class Select (f :: Type -> Type)  where
    empty :: f a
    pick :: f a -> f a -> f a

instance Select Maybe where
    empty = Nothing 
    pick Nothing a = a
    pick a _ = a

instance Select [] where
    empty = []
    pick = (<>)


-- Deriving Via Example

-- Define a newtype wrapper for Maybe in order to define a Semigroup and Moooid instance

newtype MyMaybe' a = MyMaybe' (Maybe a) deriving Show

instance Semigroup (MyMaybe' a) where
    (MyMaybe' a) <> (MyMaybe' b) = MyMaybe' (pick a b)

instance Monoid (MyMaybe' a) where
    mempty = MyMaybe' empty

-- Now we can chain MyMaybe's and the result with be the first non-Nothing:
-- MyMaybe' Nothing <> MyMaybe' (Just "a") <> MyMaybe' (Just "b") ==> MyMaybe' (Just "a")


-- Now use DerivingVia

-- Define a wrapper to define the template for derving Semigroup and Monoid instances from Select
newtype Sel (f :: Type -> Type) (a :: Type) = Sel (f a)

-- Semigroup and monoid instances which use Select 
instance (Select f) => Semigroup (Sel f a) where
    (Sel x) <> (Sel y) = Sel (pick x y) 

instance (Select f) => Monoid (Sel f a) where
    mempty = Sel empty

-- Now derive Semigroup and Monoid instrances for MyMaybe

newtype MyMaybe a = MyMaybe (Maybe a)
    deriving Show
    deriving (Semigroup, Monoid) via (Sel Maybe a)





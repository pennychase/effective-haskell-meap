{-# LANGUAGE KindSignatures #-}

module Selector where

import Data.Kind


-- Select is a simplified version of Alternative

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


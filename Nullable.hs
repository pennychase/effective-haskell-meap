{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}

module Nullable where

import Prelude hiding (null)
import Data.Kind

-- lists are nullable, so when testing isNull and isNull' for Maybe and tuples, you need to apply 
-- saturated lists:
--      isNull (Nothing :: Maybe [Int]) ==> True
--      isNull (Just [1]) ==> False
--      isNull' ([],[]) ==> True

-- Part 1: define Nullable and Maybe, tuple, list instance

class Nullable a where
    isNull :: a -> Bool
    null :: a

instance Nullable a => Nullable (Maybe a) where
    null = Nothing 
    isNull Nothing = True
    isNull _ = False

instance (Nullable a, Nullable b) => Nullable (a, b) where
    null = (null, null)
    isNull (a, b) = isNull a && isNull b

instance Nullable [a] where
    null = []
    isNull [] = True
    isNull _  = False


-- Part 2: Eq insatcen and default isNull

class Eq a => Nullable' a where
    null' :: a 
    isNull' :: a -> Bool 
    isNull' x = x == null'

instance Nullable' a =>  Nullable' (Maybe a) where
    null' = Nothing 

instance (Nullable' a, Nullable' b) => Nullable' (a, b) where
    null' = (null', null')

instance Eq a => Nullable' [a] where
    null' = []


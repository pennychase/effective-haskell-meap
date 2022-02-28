{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter5 where

import Peano

-- Chapter 5: Typeclasses and Peano

-- The Natural typeclass (with Eq constraint)

class Eq n => Natural n where
    add :: n -> n -> n
    mul :: n -> n -> n
    addIdentity :: n
    mulIdentity :: n

-- Peano instances

instance Eq Peano where
    (==) = eqPeano
    
instance Natural Peano where
    add = addPeano
    mul = mulPeano
    addIdentity = Z
    mulIdentity = S Z

-- Type Applications

instance Natural Int where
    add = (+)
    mul = (*)
    addIdentity = 0
    mulIdentity = 1

showIdentities :: forall a. (Natural a, Show a) => IO (a, a)
showIdentities = do
    let 
        mulId = mulIdentity @a
        addId = addIdentity @a
        msg = "The additive identity is: "
              <> show addId 
              <> " and the multiplicative identity is: "
              <> show mulId
    print msg
    return (addId, mulId)

adheresToReadShowContract :: forall a. (Read a, Show a) => a -> Bool
adheresToReadShowContract val =
    let
        x = show . read @a . show $ val
        y = show val
    in x == y





module Main where

import qualified Examples.CreatingModules as Ex

main :: IO ()
main = do
  Ex.publicFunction
  Ex.privateFunction

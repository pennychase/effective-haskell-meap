module Examples.CreatingModules (publicFunction) where

publicFunction :: IO ()
publicFunction = print "I'm available"

privateFunction :: IO ()
privateFunction = print "I'm not available outside this module"
module Application.Types.CustomerInfo 
    ( CustomerInfo(..), defaultCustomer ) where

data CustomerInfo = CustomerInfo
    { firstName :: String 
    , lastName :: String 
    , widgetCount :: Int 
    , balance :: Double 
    }

defaultCustomer = 
    CustomerInfo { firstName = ""
                 , lastName = ""
                 , widgetCount = 0
                 , balance = 0.0
                 }
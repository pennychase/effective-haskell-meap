module Application.Types.EmployeeInfo
    ( EmployeeInfo(..) ) where

data EmployeeInfo = EmployeeInfo
    { firstName :: String 
    , lastName :: String 
    , timezone :: String 
    , contact :: String 
    }
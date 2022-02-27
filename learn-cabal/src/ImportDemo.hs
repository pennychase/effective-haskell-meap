module ImportDemo where

import qualified Application.Types.CustomerInfo as Customer
import qualified Application.Types.EmployeeInfo as Employee

george :: Customer.CustomerInfo 
george = Customer.CustomerInfo 
    { Customer.firstName = "George"
    , Customer.lastName = "Washington"
    , Customer.widgetCount = 50
    , Customer.balance = 16750.00  
    }

thomas :: Customer.CustomerInfo 
thomas = Customer.defaultCustomer
    { Customer.firstName = "Thomas"
    , Customer.lastName = "Jefferson"
    }

james :: Employee.EmployeeInfo
james = Employee.EmployeeInfo
    { Employee.firstName = "James"
    , Employee.lastName = "Bowen"
    , Employee.timezone = "PT"
    , Employee.contact = "888-515-1212"
    }
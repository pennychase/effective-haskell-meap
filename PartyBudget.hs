module PartyBudget where

import Data.List

-- First version - food cost based on person
-- Run as: partyBudget (checkGuestList ["Ren", "Porter"]) foodCostsPerson

checkGuestList :: [String] -> String -> Bool
checkGuestList guestList name =
    name `elem` guestList

partyBudget isAttending =
    foldl (+) 0 . map snd . filter (isAttending . fst)

foodCostsPerson :: [(String, Double)]
foodCostsPerson =
    [ ("Ren", 10.00)
    , ("George", 4.00)
    , ("Porter", 27.50)
    ]

-- Second versio - food cost based on food
-- Run as: partyBudget (checkGuestList ["Ren", "Porter"]) (willEat foodsWillEat) (foodCost foodCosts) [("Ren","salad"),("Porter","steak")]

partyBudget' isAttending willEat foodCost guests =
    foldl (+) 0 $
    [ foodCost food
    | guest <- map fst guests
    , food <- map snd guests
    , willEat guest food
    , isAttending guest
    ]

foodCost foodCosts food = 
    case lookup food foodCosts of
        Nothing -> 0.0
        Just cost -> cost        

willEat foodsWillEat guest food = 
    case lookup guest foodsWillEat of
        Nothing -> False 
        Just food' -> food == food'

foodCosts = [ ("pizza", 10.0)
            , ("hamburger", 7.50)
            , ("salad", 8.5)
            , ("steak", 25.50)
            ]

foodsWillEat =  [ ("Ren", "salad")
                , ("George", "pizza")
                , ("Porter", "steak")]
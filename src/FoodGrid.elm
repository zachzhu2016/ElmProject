module FoodGrid exposing (FoodGrid(..), fromList, map, values)

-- TYPES


type FoodGrid a
    = FoodGrid
        { values : List a
        }



-- INFO


values : FoodGrid a -> List a
values (FoodGrid info) =
    info.values



-- CREATE


fromList : List a -> FoodGrid a
fromList list =
    FoodGrid { values = list }



-- TRANSFORM


map : (a -> a) -> FoodGrid a -> FoodGrid a
map transform (FoodGrid info) =
    FoodGrid { info | values = List.map transform info.values }

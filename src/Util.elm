module Util exposing (..)


import Random
zip : List a -> List b -> List (a, b)
zip l1 l2 =
    case (l1, l2) of
        (h1::t1, h2::t2) -> (h1, h2) :: (zip t1 t2)
        default -> []


zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 l1 l2 l3 =
    case (l1, l2, l3) of
        (h1::t1, h2::t2, h3::t3) -> (h1, h2, h3) :: (zip3 t1 t2 t3)
        default -> []


zipListWithNeighbours : List a -> List (Maybe a, a, Maybe a)
zipListWithNeighbours list =
    let
        justs = List.map (Just) list
        fronts = Nothing :: justs
        backs = List.drop 1 (justs ++ [ Nothing ])
    in
        zip3 fronts list backs


zmap : List a -> (a -> b) -> List (a, b)
zmap list f = zip list (List.map f list)


get : Int -> List a -> Maybe a
get index = List.head << List.drop index

randomElement : a -> List a -> Random.Generator a
randomElement default list =
    Random.int 0 ( List.length list )
    |> Random.map (flip get list)
    |> Random.map ( Maybe.withDefault default )

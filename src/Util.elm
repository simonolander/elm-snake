module Util exposing (..)

import Random


{-| zip [1, 2, 3] ["a", "b"]
        == [(1, "a"), (2, "b")]
-}
zip : List a -> List b -> List (a, b)
zip l1 l2 =
    case (l1, l2) of
        (h1::t1, h2::t2) -> (h1, h2) :: (zip t1 t2)
        default -> []


{-| zip3 [1, 2, 3] ["a", "b"] [True, False]
           == [(1, "a", True), (2, "b", False)]
-}
zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 l1 l2 l3 =
    case (l1, l2, l3) of
        (h1::t1, h2::t2, h3::t3) -> (h1, h2, h3) :: (zip3 t1 t2 t3)
        default -> []


{-| zipListWithNeighbours [1, 2, 3]
        == [ (Nothing, 1, Just 2)
           , (Just 1, 2, Just 3)
           , (Just 2, 3, Nothing)
           ]
-}
zipListWithNeighbours : List a -> List (Maybe a, a, Maybe a)
zipListWithNeighbours list =
    let
        justs = List.map (Just) list
        fronts = Nothing :: justs
        backs = List.drop 1 (justs ++ [ Nothing ])
    in
        zip3 fronts list backs


{-| zmap [1, 2, 3] toString
        == [ (1, "1")
           , (2, "2")
           , (3, "3")
           ]
-}
zmap : List a -> (a -> b) -> List (a, b)
zmap list f = zip list (List.map f list)


{-| get 1 [1, 2, 3]
        == Just 2
-}
get : Int -> List a -> Maybe a
get index = List.head << List.drop index


{-| Pick a random element from the list, or the default if the list is empty.
-}
randomElement : a -> List a -> Random.Generator a
randomElement default list =
    Random.int 0 ( List.length list )
    |> Random.map (flip get list)
    |> Random.map ( Maybe.withDefault default )


{-| flatten [[1, 2], [], [3]]
        == [1, 2, 3]
-}
flatten : List (List a) -> List a
flatten list =
    case list of
        [] -> []
        []::t -> flatten t
        (h::t)::t2 -> h :: (flatten (t::t2))


{-| comb2 [1, 2] ["a", "b", "c"]
        == [ (1, "a")
           , (1, "b")
           , (1, "c")
           , (2, "a")
           , (2, "b")
           , (2, "c")
           ]
-}
comb2 : List a -> List b -> List (a, b)
comb2 listA listB =
    case listA of
        [] -> []
        a::t ->
            (List.map ((,) a) listB) ++ (comb2 t listB)

module Helpers exposing (..)

{-| Random non-project specific helper functions
-}


{-| Return Just x if the input is [x], Nothing otherwise. Useful for checking that a given list has exactly one element
and getting that element.

    ensureSingleton [ 1 ] == Just 1
    ensureSingleton [] == Nothing
    ensureSingleton [ 1, 2 ] = Nothing

-}
ensureSingleton : List a -> Maybe a
ensureSingleton xs =
    if List.length xs > 1 then
        Nothing
    else
        List.head xs


{-| Return [x] if the input is Just x, [] otherwise.

    listFromMaybe (Just 1) == [ 1 ]
    listFromMaybe Nothing == []

-}
listFromMaybe : Maybe a -> List a
listFromMaybe x =
    Maybe.withDefault [] (Maybe.map List.singleton x)


{-| Removes all the Nothings from the list, and unwraps the rest.

    flattenMaybeList [Nothing, Just 1] == [ 1 ]
    flattenMaybeList [] == []
    flattenMaybeList [Nothing, Nothing] == []
    flattenMaybeList [Just 1, Just 2] == [ 1, 2 ]

-}
flattenMaybeList : List (Maybe a) -> List a
flattenMaybeList =
    List.concatMap listFromMaybe

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


{-| Returns the element at the `idx`'th position (0 based) in the list, if exists. `idx` must be non-negative (reverse
indexing is not supported). O(N)!

    listElemAtIndex 2 ["a", "b", "c", "d"] == Just "c"
    listElemAtIndex 4 ["a", "b", "c", "d"] == Nothing
    listElemAtIndex -1 ["a", "b", "c", "d"] == Nothing

-}
listElemAtIndex : Int -> List a -> Maybe a
listElemAtIndex idx xs =
    List.take (idx + 1) xs
        |> List.reverse
        |> List.head


{-| Returns the first index (0 based) at which `predicate` is true, or Nothing. O(N)!

    even = \x -> x % 2 == 0
    firstIndexOf even [1, 2, 3, 5] == Just 1
    firstIndexOf even [1, 3, 5, 7] == Nothing
    firstIndexOf even [] == Nothing

-}
firstIndexOf : (a -> Bool) -> List a -> Maybe Int
firstIndexOf predicate xs =
    let
        firstIndexOfRec =
            \i ys ->
                case ys of
                    y :: zs ->
                        if predicate y then
                            Just i
                        else
                            firstIndexOfRec (i + 1) zs

                    [] ->
                        Nothing
    in
        firstIndexOfRec 0 xs


{-| Same as Maybe.andThen but it takes 2 arguments. Both maybe arguments have to be Just, or else the result is Nothing.

    even = \x -> x % 2 == 0
    sumIfEven a b =
        let
            res = a + b
        in
            if even res then
                Just res
            else
                Nothing
    maybeAndThen2 sumIfEven (Just 1) (Just 3) == Just 4
    maybeAndThen2 sumIfEven (Just 1) (Just 4) == Nothing
    maybeAndThen2 sumIfEven Nothing (Just 3) == Nothing
    maybeAndThen2 sumIfEven (Just 1) Nothing == Nothing

-}
maybeAndThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
maybeAndThen2 f x y =
    x
        |> Maybe.andThen (\justX -> Maybe.map (\justY -> ( justX, justY )) y)
        |> Maybe.andThen (uncurry f)

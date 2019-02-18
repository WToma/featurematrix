module Helpers exposing (..)

{-| Random non-project specific helper functions
-}

import Dict
import Char


{-| Return Just x if the input is [x], Nothing otherwise. Useful for checking that a given list has exactly one element
and getting that element.

    ensureSingleton [ 1 ] --> Just 1
    ensureSingleton [] --> Nothing
    ensureSingleton [ 1, 2 ] --> Nothing

-}
ensureSingleton : List a -> Maybe a
ensureSingleton xs =
    if List.length xs > 1 then
        Nothing
    else
        List.head xs


{-| Return [x] if the input is Just x, [] otherwise.

    listFromMaybe (Just 1) --> [ 1 ]
    listFromMaybe Nothing --> []

-}
listFromMaybe : Maybe a -> List a
listFromMaybe x =
    Maybe.withDefault [] (Maybe.map List.singleton x)


{-| Removes all the Nothings from the list, and unwraps the rest.

    flattenMaybeList [Nothing, Just 1] --> [ 1 ]
    flattenMaybeList [] --> []
    flattenMaybeList [Nothing, Nothing] --> []
    flattenMaybeList [Just 1, Just 2] --> [ 1, 2 ]

-}
flattenMaybeList : List (Maybe a) -> List a
flattenMaybeList =
    List.concatMap listFromMaybe


{-| Returns the element at the `idx`'th position (0 based) in the list, if exists. `idx` must be non-negative (reverse
indexing is not supported). O(N)!

    listElemAtIndex 2 ["a", "b", "c", "d"] --> Just "c"
    listElemAtIndex 4 ["a", "b", "c", "d"] --> Nothing
    listElemAtIndex -1 ["a", "b", "c", "d"] --> Nothing

-}
listElemAtIndex : Int -> List a -> Maybe a
listElemAtIndex idx xs =
    if idx >= (List.length xs) then
        Nothing
    else
        List.take (idx + 1) xs
            |> List.reverse
            |> List.head


{-| Returns the element at the `idx`'th position (0 based) in the list, if exists, otherwise returns err.
`idx` must be non-negative (reverse indexing is not supported). O(N)!

    listElemAtIndexResult "does not exist" 2 ["a", "b", "c", "d"] --> Ok "c"
    listElemAtIndexResult "does not exist" 4 ["a", "b", "c", "d"] --> Err "does not exist"
    listElemAtIndexResult "does not exist" -1 ["a", "b", "c", "d"] --> Err "does not exist"

-}
listElemAtIndexResult : x -> Int -> List a -> Result x a
listElemAtIndexResult err idx xs =
    Result.fromMaybe err (listElemAtIndex idx xs)


{-| Returns the first index (0 based) at which `predicate` is true, or Nothing. O(N)!

    even : Int -> Bool
    even = \x -> x % 2 == 0

    firstIndexOf even [1, 2, 3, 5] --> Just 1
    firstIndexOf even [1, 3, 5, 7] --> Nothing
    firstIndexOf even [] --> Nothing

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

    even : Int -> Bool
    even = \x -> x % 2 == 0

    sumIfEven : Int -> Int -> Maybe Int
    sumIfEven a b =
        let
            res = a + b
        in
            if even res then
                Just res
            else
                Nothing

    maybeAndThen2 sumIfEven (Just 1) (Just 3) --> Just 4
    maybeAndThen2 sumIfEven (Just 1) (Just 4) --> Nothing
    maybeAndThen2 sumIfEven Nothing (Just 3) --> Nothing
    maybeAndThen2 sumIfEven (Just 1) Nothing --> Nothing

-}
maybeAndThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
maybeAndThen2 f x y =
    x
        |> Maybe.andThen (\justX -> Maybe.map (\justY -> ( justX, justY )) y)
        |> Maybe.andThen (uncurry f)


{-| Same as Result.andThen but it takes 2 arguments. If both arguments are Ok then the function is evaluated on them.
Otherwise the earlier one of the argument errors is returned.

    even : Int -> Bool
    even = \x -> x % 2 == 0

    sumIfEven : Int -> Int -> Result String Int
    sumIfEven a b =
        let
            res = a + b
        in
            if even res then
                Ok res
            else
                Err "I can't even"

    resultAndThen2 sumIfEven (Ok 1) (Ok 3) --> Ok 4
    resultAndThen2 sumIfEven (Ok 1) (Ok 4) --> Err "I can't even"
    resultAndThen2 sumIfEven (Err "not a number") (Ok 3) --> Err "not a number"
    resultAndThen2 sumIfEven (Ok 3) (Err "not a number") --> Err "not a number"
    resultAndThen2 sumIfEven (Err "not a number") (Err "ion cannon failure") --> Err "not a number"

-}
resultAndThen2 : (a -> b -> Result x c) -> Result x a -> Result x b -> Result x c
resultAndThen2 f x y =
    x
        |> Result.andThen (\okX -> Result.map (\okY -> ( okX, okY )) y)
        |> Result.andThen (uncurry f)


{-| Returns a tuple with the same elements as the original tuple, but they're in ascending order.

    orderTuple (1, 2) --> (1, 2)
    orderTuple (2, 1) --> (1, 2)
    orderTuple ("b", "a") --> ("a", "b")

-}
orderTuple : ( comparable, comparable ) -> ( comparable, comparable )
orderTuple ( x, y ) =
    if x <= y then
        ( x, y )
    else
        ( y, x )


{-| Folds over a list of results from left to right. Returns the fold result, or the first error from the input list.

    resultListFoldl (+) 0 [] --> Result.Ok 0
    resultListFoldl (::) [] [Result.Ok 1, Result.Ok 2, Result.Ok 3] --> Result.Ok [3, 2, 1]
    resultListFoldl (::) [] [Result.Ok 1, Result.Err "nope", Result.Ok 3] --> Result.Err "nope"
    resultListFoldl (::) [] [Result.Err "nada", Result.Err "nope", Result.Ok 3] --> Result.Err "nada"

-}
resultListFoldl : (okT -> accT -> accT) -> accT -> List (Result errT okT) -> Result errT accT
resultListFoldl f x0 =
    let
        -- this is very similar to (Result.map2 f), with the difference that this implementation keeps the
        -- error from acc. Since in f ok is the first argument, using Result.map2 would keep the last error
        fR xR accR =
            case accR of
                Result.Ok acc ->
                    Result.map (\x -> f x acc) xR

                err ->
                    err
    in
        List.foldl fR (Result.Ok x0)


{-| Counts the number of occurrences for each element in the list.

    import Dict

    counts ["a", "b", "a"] --> Dict.fromList [("a", 2), ("b", 1)]
    counts [] --> Dict.fromList []

-}
counts : List comparable -> Dict.Dict comparable Int
counts xs =
    let
        inc x d =
            let
                currentValue =
                    Dict.get x d |> Maybe.withDefault 0
            in
                Dict.insert x (currentValue + 1) d
    in
        List.foldl inc Dict.empty xs


{-| Performs a transformation on the head of a list, leaving the tail unchanged

    mapFirst ((+) 1) [1, 2] --> [2, 2]
    mapFirst ((+) 1) [] --> []

-}
mapFirst : (a -> a) -> List a -> List a
mapFirst f xs =
    case xs of
        [] ->
            []

        head :: tail ->
            (f head) :: tail


{-| Converts a list of words to camelCase

    phraseToCamelCase "" --> ""
    phraseToCamelCase "hello" --> "hello"
    phraseToCamelCase "hello world" --> "helloWorld"

-}
phraseToCamelCase : String -> String
phraseToCamelCase phrase =
    let
        words =
            String.words phrase

        firstCharToLower =
            \s -> String.fromList (mapFirst Char.toLocaleLower (String.toList s))

        firstCharToUpper =
            \s -> String.fromList (mapFirst Char.toLocaleUpper (String.toList s))

        allFirstUpper =
            List.map firstCharToUpper words

        camelCaseWords =
            mapFirst firstCharToLower allFirstUpper
    in
        List.foldr (++) "" camelCaseWords


{-| Returns the 2 surrounding elements around the first occurrence of a given element.

    window1 ((==) "d") ["a", "b", "c", "d", "e"] --> (Just "c", Just "d", Just "e")
    window1 ((==) "e") ["a", "b", "c", "d", "e"] --> (Just "d", Just "e", Nothing)
    window1 ((==) "d") [] --> (Nothing, Nothing, Nothing)
    window1 ((==) "d") ["notd"] --> (Nothing, Nothing, Nothing)
    window1 ((==) "d") ["d"] --> (Nothing, Just "d", Nothing)
    window1 ((==) "d") ["d", "e"] --> (Nothing, Just "d", Just "e")
    window1 ((==) "d") ["c", "d"] --> (Just "c", Just "d", Nothing)

-}
window1 : (a -> Bool) -> List a -> ( Maybe a, Maybe a, Maybe a )
window1 predicate xs =
    case xs of
        [] ->
            ( Nothing, Nothing, Nothing )

        x :: [] ->
            if (predicate x) then
                ( Nothing, Just x, Nothing )
            else
                ( Nothing, Nothing, Nothing )

        x :: y :: [] ->
            if (predicate x) then
                ( Nothing, Just x, Just y )
            else if (predicate y) then
                ( Just x, Just y, Nothing )
            else
                ( Nothing, Nothing, Nothing )

        x :: y :: z :: tail ->
            if (predicate x) then
                ( Nothing, Just x, Just y )
            else if (predicate y) then
                ( Just x, Just y, Just z )
            else
                window1 predicate (y :: z :: tail)


{-| Similar to Maybe.withDefault, except in this case the fallback is also a maybe.

    Just 9 |> maybeOrElse (Just 42) --> Just 9
    Nothing |> maybeOrElse (Just 42) --> Just 42
    Nothing |> maybeOrElse Nothing -> Nothing

-}
maybeOrElse : Maybe a -> Maybe a -> Maybe a
maybeOrElse fallback primary =
    case primary of
        Just x ->
            Just x

        Nothing ->
            fallback


{-| Drops elements from the list until it encounters an element that satisfies the predicate.

    dropUntil (\x -> x > 2) [0, 1, 2, 3, -1234] --> [3, -1234]
    dropUntil (\x -> x > 2) [] --> []
    dropUntil (\x -> x > 2) [0, 1, 2, -1234] --> []

-}
dropUntil : (a -> Bool) -> List a -> List a
dropUntil predicate xs =
    case xs of
        [] ->
            []

        head :: tail ->
            if predicate head then
                xs
            else
                dropUntil predicate tail

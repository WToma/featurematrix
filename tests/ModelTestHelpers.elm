module ModelTestHelpers exposing (..)

import Main
import Expect exposing (Expectation)
import Dict
import Helpers exposing (orderTuple, ensureSingleton, listElemAtIndex, resultListFoldl)
import TestHelpers exposing (expectNotError)


verifyIntersectionText : String -> String -> String -> Main.Model -> Expectation
verifyIntersectionText rowFeatureId colFeatureId expectedText model =
    let
        valueAtCell =
            Dict.get (orderTuple ( rowFeatureId, colFeatureId )) model.intersections

        location =
            "at intersection " ++ rowFeatureId ++ " / " ++ colFeatureId
    in
        if expectedText /= "" then
            case valueAtCell of
                Just valueAtCell ->
                    Expect.equal expectedText valueAtCell
                        |> Expect.onFail ("expected '" ++ expectedText ++ "', got '" ++ valueAtCell ++ "' " ++ location)

                Nothing ->
                    Expect.fail ("there was nothing " ++ location)
        else
            case valueAtCell of
                Just valueAtCell ->
                    Expect.fail ("entering an empty string should have weased the intesection, but got '" ++ valueAtCell ++ "' " ++ location)

                Nothing ->
                    Expect.pass


getFeature : Main.Model -> Int -> Result String Main.Feature
getFeature model randomIndex =
    let
        featureIndex =
            randomIndex % (List.length model.features)

        maybeFeature =
            listElemAtIndex featureIndex model.features
    in
        Result.fromMaybe ("no feature at index " ++ (toString featureIndex)) maybeFeature


getFeatureByName : Main.Model -> String -> Result String Main.Feature
getFeatureByName model displayName =
    List.filter (\f -> f.displayName == displayName) model.features
        |> ensureSingleton
        |> Result.fromMaybe ("feature with header \"" ++ displayName ++ "\" was not in the model")


getIntersectionByName : String -> String -> Main.Model -> Result String String
getIntersectionByName displayName1 displayName2 model =
    let
        f1 =
            getFeatureByName model displayName1 |> Result.map .featureId

        f2 =
            getFeatureByName model displayName2 |> Result.map .featureId
    in
        Result.map2 (,) f1 f2
            |> Result.map orderTuple
            |> Result.map ((flip Dict.get) model.intersections)
            |> Result.map (Maybe.withDefault "")


verifyTextContainsIntersections : String -> Main.Model -> Expectation
verifyTextContainsIntersections text model =
    let
        intersections =
            Dict.toList model.intersections

        intersectionPresent ( ( smallerKey, largerKey ) as k, intersection ) =
            if String.contains intersection text then
                Result.Ok k
            else
                Result.Err ("the value for (" ++ smallerKey ++ ", " ++ largerKey ++ ") (" ++ intersection ++ ") was not present")

        results =
            List.map intersectionPresent intersections

        result =
            resultListFoldl (always (always ())) () results
    in
        expectNotError result

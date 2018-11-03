module FeatureMatrixTests exposing (..)

import Test exposing (Test, describe, todo, test, fuzz3)
import Expect exposing (Expectation)
import Main
import Dict
import Test.Html.Event as Event
import ElmHtml.Query as ElmHtmlQuery
import HtmlTestExtra
import ElmHtml.InternalTypes exposing (ElmHtml)
import Set exposing (Set)
import Helpers exposing (..)
import Html exposing (Html)
import Fuzz
import TableViewHelpers exposing (columnHeaderNames, rowHeaderNames, findFeatureTableElmHtml, findIntersectionCell)


tableModeShowsAllFeatures : Test
tableModeShowsAllFeatures =
    describe "There is Table Mode in which all feature intersections can be seen."
        [ test "all column header names are shown in the initial view" <|
            \() -> expectHeaderNamesToBe columnHeaderNames "column" dummyFeatureDisplayNames
        , test "all row header names are shown in the initial view" <|
            \() -> expectHeaderNamesToBe rowHeaderNames "row" dummyFeatureDisplayNames
        , test "column and row headers are shown in the same order" <|
            \() ->
                testInitialTable <|
                    \table ->
                        let
                            columnHeaders =
                                columnHeaderNames table
                                    |> Result.fromMaybe "column headers not found"

                            rowHeaders =
                                rowHeaderNames table
                                    |> Result.fromMaybe "row headers not found"
                        in
                            Result.map2 Expect.equalLists columnHeaders rowHeaders
        , todo "feature intersections can be seen in the table"
        , todo "feature intersections are diagonally mirrored"
        ]


editIntersection : Test
editIntersection =
    describe "In Table View I can type in an intersection of two features."
        [ fuzz3 Fuzz.int Fuzz.int Fuzz.string "typing in an intersection is reflected in the model" <|
            \rowRandom colRandom intersectionText ->
                let
                    model =
                        initialModel

                    numFeatures =
                        List.length model.features

                    toFeatureIndex =
                        \i ->
                            let
                                modulo =
                                    i % numFeatures
                            in
                                if modulo >= 0 then
                                    modulo
                                else
                                    modulo + numFeatures

                    pickFeatureFromRandom =
                        \i ->
                            let
                                featureIndex =
                                    toFeatureIndex i
                            in
                                case listElemAtIndex featureIndex model.features of
                                    Nothing ->
                                        Debug.crash ("no feature at index " ++ (toString featureIndex))

                                    Just f ->
                                        f

                    rowFeature =
                        pickFeatureFromRandom rowRandom

                    colFeature =
                        pickFeatureFromRandom colRandom
                in
                    initialView
                        |> HtmlTestExtra.fromHtml
                        |> findFeatureTableElmHtml
                        |> Result.fromMaybe "could not find feature table"
                        |> Result.andThen (typeIntoIntersection rowFeature.displayName colFeature.displayName intersectionText)
                        |> Result.map ((flip Main.update) initialModel)
                        |> Result.map (verifyIntersectionText rowFeature.featureId colFeature.featureId intersectionText)
                        |> resultToExpectation
        ]


initialView : Html Main.Msg
initialView =
    Main.view initialModel


initialModel : Main.Model
initialModel =
    { features = Main.dummyFeatures
    , featureVisibility = Main.allFeaturesVisible Main.dummyFeatures
    , intersections = Dict.empty
    , parseError = Nothing
    , showSerialized = False
    , newFeaturePanelState =
        { shortName = ""
        , description = ""
        , errorAdding = Nothing
        }
    }


testInitialTable : (ElmHtml Main.Msg -> Result String Expectation) -> Expectation
testInitialTable t =
    let
        table =
            findFeatureTableElmHtml (HtmlTestExtra.fromHtml initialView)
                |> Result.fromMaybe "feature table not found"

        testResult =
            Result.andThen t table
    in
        resultToExpectation testResult


dummyFeatureDisplayNames : List String
dummyFeatureDisplayNames =
    List.map (\df -> df.displayName) Main.dummyFeatures


expectHeaderNamesToBe : (ElmHtml Main.Msg -> Maybe (List String)) -> String -> List String -> Expectation
expectHeaderNamesToBe getHeadersFn nameInErr expectedHeaderNames =
    testInitialTable <|
        \table ->
            getHeadersFn table
                |> Result.fromMaybe (nameInErr ++ " headers not found")
                |> Result.map Set.fromList
                |> Result.map (Expect.equalSets (Set.fromList expectedHeaderNames))


typeIntoIntersection : String -> String -> String -> ElmHtml Main.Msg -> Result String Main.Msg
typeIntoIntersection rowLabel colLabel textToInsert html =
    let
        event =
            Event.input textToInsert

        errorText =
            "could not find the cell for '" ++ rowLabel ++ "' / '" ++ colLabel ++ "' in the feature table"

        textField : ElmHtml Main.Msg -> Maybe (ElmHtml Main.Msg)
        textField intersectionCell =
            intersectionCell
                |> ElmHtmlQuery.queryByTagName "textarea"
                |> ensureSingleton
    in
        findIntersectionCell rowLabel colLabel html
            |> Result.andThen (\cell -> Result.fromMaybe "could not find input field in cell" <| textField cell)
            |> Result.andThen (HtmlTestExtra.simulate event)


verifyIntersectionText : String -> String -> String -> Main.Model -> Expectation
verifyIntersectionText rowFeatureId colFeatureId expectedText model =
    let
        ( smallerKey, largerKey ) =
            if rowFeatureId < colFeatureId then
                ( rowFeatureId, colFeatureId )
            else
                ( colFeatureId, rowFeatureId )

        valueAtCell =
            Dict.get ( smallerKey, largerKey ) model.intersections

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


resultToExpectation : Result String Expectation -> Expectation
resultToExpectation res =
    case res of
        Err reason ->
            Expect.fail reason

        Ok expectation ->
            expectation

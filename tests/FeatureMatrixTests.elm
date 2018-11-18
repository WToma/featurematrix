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
import TableViewHelpers exposing (columnHeaderNames, rowHeaderNames, findFeatureTableElmHtml, findIntersectionCell, findTextFieldInCell)


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
        , test "feature intersections can be seen in the table" <|
            \() ->
                let
                    verify : ElmHtml msg -> Result String Expectation
                    verify table =
                        verifyAllIntersectionsInTable initialModel table |> Result.Ok
                in
                    testInitialTable <|
                        \table -> verify table

        -- , todo "feature intersections are diagonally mirrored"
        -- this is actually tested by the fact that we test all intersections in the above test
        -- and we verified previously that the headers are shown in the same order in rows and columns
        ]


editIntersection : Test
editIntersection =
    describe "In Table View I can type in an intersection of two features."
        [ fuzz3 Fuzz.int Fuzz.int Fuzz.string "typing in an intersection is reflected in the model" <|
            \rowRandom colRandom intersectionText ->
                let
                    testRowAndCol rowFeature colFeature table =
                        typeIntoIntersection rowFeature.displayName colFeature.displayName intersectionText table
                            |> Result.map ((flip Main.update) initialModel)
                            |> Result.map (verifyIntersectionText rowFeature.featureId colFeature.featureId intersectionText)

                    doTest : ElmHtml Main.Msg -> Result String Expectation
                    doTest table =
                        resultAndThen2
                            (\r c -> testRowAndCol r c table)
                            (getFeature initialModel rowRandom)
                            (getFeature initialModel colRandom)
                in
                    testInitialTable doTest
        ]


initialView : Html Main.Msg
initialView =
    Main.view initialModel


dummyIntersections : Dict.Dict ( String, String ) String
dummyIntersections =
    Dict.fromList
        [ ( ( "edit", "showFeatures" ), "edited features are displayed" )
        , ( ( "edit", "export" ), "edited features are exported" )
        , ( ( "addFeature", "import" ), "added features can be exported and imported" )
        ]


initialModel : Main.Model
initialModel =
    { features = Main.dummyFeatures
    , featureVisibility = Main.allFeaturesVisible Main.dummyFeatures
    , intersections = dummyIntersections
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
    in
        findIntersectionCell rowLabel colLabel html
            |> Result.andThen findTextFieldInCell
            |> Result.andThen (HtmlTestExtra.simulate event)


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


verifyAllIntersectionsInTable : Main.Model -> ElmHtml msg -> Expectation
verifyAllIntersectionsInTable model table =
    let
        featureIds =
            List.map .featureId model.features

        allIntersections =
            List.concatMap (\f1 -> List.map ((,) f1) featureIds) featureIds

        allExpectations : List (ElmHtml msg -> Expectation)
        allExpectations =
            List.map
                (\( f1, f2 ) -> \table -> resultToExpectation (verifyIntersectionTextInTable f1 f2 model table))
                allIntersections
    in
        Expect.all allExpectations table


verifyIntersectionTextInTable : String -> String -> Main.Model -> ElmHtml msg -> Result String Expectation
verifyIntersectionTextInTable rowFeatureId colFeatureId model table =
    let
        featureIdToFeatureMap =
            Dict.fromList <| List.map (\f -> ( f.featureId, f )) model.features

        rowHeaderName : Result String String
        rowHeaderName =
            Dict.get rowFeatureId featureIdToFeatureMap
                |> Result.fromMaybe ("no such feature in model for row feature ID: " ++ rowFeatureId)
                |> Result.map .displayName

        colHeaderName : Result String String
        colHeaderName =
            Dict.get colFeatureId featureIdToFeatureMap
                |> Result.fromMaybe ("no such feature in model for column feature ID: " ++ colFeatureId)
                |> Result.map .displayName

        expectedText : String
        expectedText =
            Dict.get (orderTuple ( rowFeatureId, colFeatureId )) model.intersections
                |> Maybe.withDefault ""

        verify : String -> String -> Result String Expectation
        verify row col =
            findIntersectionCell row col table
                |> Result.andThen findTextFieldInCell
                |> Result.map HtmlTestExtra.getAttributes
                |> Result.map (getStringAttribute "value")
                |> Result.map (Maybe.withDefault "")
                |> Result.map (Expect.equal expectedText)
    in
        resultAndThen2 verify rowHeaderName colHeaderName


resultToExpectation : Result String Expectation -> Expectation
resultToExpectation res =
    case res of
        Err reason ->
            Expect.fail reason

        Ok expectation ->
            expectation


getStringAttribute : String -> ( Dict.Dict String String, a ) -> Maybe String
getStringAttribute attribute ( stringAttributes, _ ) =
    Dict.get attribute stringAttributes


getBoolAttribute : String -> ( a, Dict.Dict String Bool ) -> Maybe Bool
getBoolAttribute attribute ( _, boolAttributes ) =
    Dict.get attribute boolAttributes


getFeature : Main.Model -> Int -> Result String Main.Feature
getFeature model randomIndex =
    let
        featureIndex =
            randomIndex % (List.length model.features)

        maybeFeature =
            listElemAtIndex featureIndex model.features
    in
        Result.fromMaybe ("no feature at index " ++ (toString featureIndex)) maybeFeature

module FeatureMatrixTests exposing (..)

import Test exposing (Test, describe, todo, test, fuzz, fuzz3)
import Expect exposing (Expectation)
import Main
import Dict
import Test.Html.Query as Query
import Test.Html.Event as Event
import Test.Html.Selector as Selector exposing (text, tag, class)
import ElmHtml.Query as ElmHtmlQuery
import HtmlTestExtra
import ElmHtml.InternalTypes exposing (ElmHtml)
import Set exposing (Set)
import Helpers exposing (..)
import Html exposing (Html)
import Fuzz
import Fuzzers exposing (modelFuzzerAllVisible)
import Time
import DebugTime


tableModeShowsAllFeatures : Test
tableModeShowsAllFeatures =
    describe "There is Table Mode in which all feature intersections can be seen."
        [ test "all column and row headers are shown in the initial view"
            (\() -> testColumnAndRowHeaderNamesForModel initialModel)
        , Test.only (fuzz modelFuzzerAllVisible
            "all column and row headers can be seen in a random model (fuzz)" <|
                DebugTime.printElapsedTime1
                    "fuzz test run"
                    (\model -> testColumnAndRowHeaderNamesForModel model))
        , todo "column and row headers are shown in the same order"
        , todo "feature intersections can be seen in the table"
        , todo "feature intersections are diagonally mirrored"
        ]


testColumnAndRowHeaderNamesForModel : Main.Model -> Expectation
testColumnAndRowHeaderNamesForModel model =
    let
        viewResult =
            Main.view model

        expectedHeaderNames =
            getFeatureDisplayNames model
    in
        testColumnAndRowHeaderNames expectedHeaderNames viewResult


testColumnAndRowHeaderNames : List String -> Html msg -> Expectation
testColumnAndRowHeaderNames expectedHeaderNames viewResult =
    Expect.all
        [ expectColumnHeaderNames expectedHeaderNames, expectRowHeaderNames expectedHeaderNames ]
        viewResult


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

                    -- TODO use Fuzz.intRange instead
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


getFeatureDisplayNames : Main.Model -> List String
getFeatureDisplayNames model =
    List.map (\df -> df.displayName) model.features


expectColumnHeaderNames : List String -> Html msg -> Expectation
expectColumnHeaderNames expectedHeaderNames viewResult =
    viewResult
        |> Query.fromHtml
        |> findFeatureTable
        |> columnHeaderNamesContain expectedHeaderNames


columnHeaderNamesContain : List String -> Query.Single msg -> Expectation
columnHeaderNamesContain featureDisplayNames html =
    let
        columnHeaderSelector featureDisplayName =
            Selector.containing [ tag "th", Selector.containing [ text featureDisplayName ] ]

        allColumnHeadersSelector =
            List.map columnHeaderSelector featureDisplayNames
    in
        findColumnHeaderRow html
            |> Query.has allColumnHeadersSelector


findColumnHeaderRow : Query.Single msg -> Query.Single msg
findColumnHeaderRow html =
    html
        |> Query.findAll [ tag "tr" ]
        |> Query.first


findFeatureTable : Query.Single msg -> Query.Single msg
findFeatureTable html =
    -- note: for whatever reason the order of attributes matter here; if we specify `tag` first and `class` second, it'll find
    -- a bunch more elements
    Query.find [ class "featureTable", tag "table" ] html


expectRowHeaderNames : List String -> Html msg -> Expectation
expectRowHeaderNames expectedHeaderNames viewResult =
    viewResult
        |> HtmlTestExtra.fromHtml
        |> findFeatureTableElmHtml
        |> Maybe.andThen rowHeaderNames
        |> \actual -> Expect.equalSets (Set.fromList expectedHeaderNames) (Maybe.withDefault Set.empty actual)


findFeatureTableElmHtml : ElmHtml msg -> Maybe (ElmHtml msg)
findFeatureTableElmHtml html =
    let
        results =
            html
                -- same note as in findFeatureTable: the order of selectors matters a lot unfortunately
                |> ElmHtmlQuery.queryByClassName "featureTable"
                |> List.concatMap (\elemWithClassName -> ElmHtmlQuery.queryByTagName "table" elemWithClassName)
    in
        ensureSingleton results


rowHeaderNames : ElmHtml msg -> Maybe (Set String)
rowHeaderNames table =
    let
        getRowHeader row =
            row
                |> ElmHtmlQuery.queryByTagName "th"
                |> List.concatMap (ElmHtmlQuery.queryByTagName "div")
                |> List.head
                |> Maybe.map HtmlTestExtra.extractText
                |> Maybe.andThen ensureSingleton

        rowHeaders =
            table
                |> ElmHtmlQuery.queryByTagName "tr"
                |> List.tail
                -- ignore the first row, which contains the column headers
                |> Maybe.map (List.map getRowHeader)
    in
        rowHeaders
            |> Maybe.map flattenMaybeList
            |> Maybe.map Set.fromList


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
            |> Result.fromMaybe errorText
            |> Result.andThen (\cell -> Result.fromMaybe "could not find input field in cell" <| textField cell)
            |> Result.andThen (HtmlTestExtra.simulate event)


findIntersectionCell : String -> String -> ElmHtml msg -> Maybe (ElmHtml msg)
findIntersectionCell rowLabel colLabel featureTable =
    let
        rows =
            ElmHtmlQuery.queryByTagName "tr" featureTable

        columnHeaders : Maybe (List (ElmHtml msg))
        columnHeaders =
            rows
                |> List.head
                -- first row
                |> Maybe.map (ElmHtmlQuery.queryByTagName "th")
                |> Maybe.andThen List.tail

        -- get rid of the first cell
        hasText : String -> ElmHtml msg -> Bool
        hasText text headerCell =
            ElmHtmlQuery.queryByTagName "div" headerCell
                |> List.concatMap HtmlTestExtra.extractText
                |> ensureSingleton
                |> Maybe.map ((==) text)
                |> Maybe.withDefault False

        columnHeaderIndex =
            columnHeaders
                |> Maybe.andThen (firstIndexOf (hasText colLabel))

        rowHeaderHasText text row =
            row
                |> ElmHtmlQuery.queryByTagName "th"
                |> List.head
                |> Maybe.map (hasText text)
                |> Maybe.withDefault False

        row =
            rows
                |> List.filter (rowHeaderHasText rowLabel)
                |> List.head
    in
        row
            |> Maybe.map (ElmHtmlQuery.queryByTagName "td")
            |> maybeAndThen2 listElemAtIndex columnHeaderIndex


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

module FeatureMatrixTests exposing (..)

import Test exposing (Test, describe, todo, test, fuzz3)
import Expect exposing (Expectation)
import Main
import Dict
import Test.Html.Event as Event
import HtmlTestExtra
import ElmHtml.InternalTypes exposing (ElmHtml)
import Set exposing (Set)
import Helpers exposing (..)
import Fuzz
import TableViewHelpers exposing (columnHeaderNames, rowHeaderNames, findFeatureTableElmHtml, findIntersectionCell, findTextFieldInCell)
import ControlPanelHelpers exposing (findShowHideModelButton, findExportImportTextField)
import Json.Encode as JE


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
        [ typeIntoIntersectionTest "typing in an intersection is reflected in the model" initialModel Result.Ok <|
            \rowId colId text model -> verifyIntersectionText rowId colId text model |> Result.Ok

        -- todo "If I type in a field other than the main diagonal, it'll appear in the transposition of the cell I typed into as well."
        -- this is implicitly tested by the fact that we have already tested that the display is simmetrical
        ]


export : Test
export =
    describe "There is a button that allows me to view a text representation of my project."
        [ test "the button can be clicked and then a textbox appears with the text representation" <|
            \() ->
                let
                    updatedView =
                        showImportExportPanel initialModel |> Result.map render

                    content =
                        Result.andThen getImportExportContent updatedView

                    verification actual =
                        verifyTextContainsIntersections actual initialModel
                in
                    content
                        |> Result.map verification
                        |> resultToExpectation
        , typeIntoIntersectionTest "if I edit an intersection, it's represented in the exportable model" initialModel Result.Ok <|
            \rowId colId text model ->
                let
                    updatedView =
                        showImportExportPanel model |> Result.map render

                    content =
                        Result.andThen getImportExportContent updatedView
                in
                    verifyShownModelContainsTypedIntersection text content
        , typeIntoIntersectionTest "if I edit an intersection while the export textbox is shown, the textbox updates immediately" initialModel showImportExportPanel <|
            \rowId coldId text model ->
                let
                    content =
                        render model
                            |> getImportExportContent
                in
                    verifyShownModelContainsTypedIntersection text content
        ]


importFeature : Test
importFeature =
    describe "If I change the state of the text field showing the exported model I instruct the system to load the new representation."
        [ test "when the import text is changed to a valid string, the intersections are reflected in the feature table" <|
            \() ->
                (importAndFindFeatureTable "[{\"smallerKey\":\"import\",\"largerKey\":\"showFeatures\",\"value\":\"previously saved content\"}]" initialModel)
                    |> Result.map
                        (expectIntersections
                            [ ( "Import", "Shows Features", "previously saved content" )
                            , ( "Edit Intersection", "Export", "" )
                            ]
                        )
                    |> resultToExpectation
        , test "when the import text is changed to an invalid string, the intersections remain the same" <|
            \() ->
                (importAndFindFeatureTable "not a valid JSON" initialModel)
                    |> Result.map
                        (expectIntersections
                            [ ( "Import", "Shows Features", "" )
                            , ( "Edit Intersection", "Export", "edited features are exported" )
                            ]
                        )
                    |> resultToExpectation
        ]


render : Main.Model -> ElmHtml Main.Msg
render model =
    Main.view model |> HtmlTestExtra.fromHtml


initialView : ElmHtml Main.Msg
initialView =
    render initialModel


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
    testTable initialModel t


testTable : Main.Model -> (ElmHtml Main.Msg -> Result String Expectation) -> Expectation
testTable initialModel t =
    let
        table =
            initialModel
                |> render
                |> findFeatureTableElmHtml
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


typeIntoIntersectionTest : String -> Main.Model -> (Main.Model -> Result String Main.Model) -> (String -> String -> String -> Main.Model -> Result String Expectation) -> Test
typeIntoIntersectionTest title modelBeforePreamble preamble verification =
    fuzz3 Fuzz.int Fuzz.int Fuzz.string title <|
        \rowRandom colRandom intersectionText ->
            let
                initialModel =
                    preamble modelBeforePreamble

                testRowAndCol : Main.Model -> Main.Feature -> Main.Feature -> ElmHtml Main.Msg -> Result String Expectation
                testRowAndCol initialModel rowFeature colFeature table =
                    typeIntoIntersection rowFeature.displayName colFeature.displayName intersectionText table
                        |> Result.map ((flip Main.update) initialModel)
                        |> Result.andThen (verification rowFeature.featureId colFeature.featureId intersectionText)

                doTest : Main.Model -> ElmHtml Main.Msg -> Result String Expectation
                doTest initialModel table =
                    resultAndThen2
                        (\r c -> testRowAndCol initialModel r c table)
                        (getFeature initialModel rowRandom)
                        (getFeature initialModel colRandom)
            in
                preamble modelBeforePreamble
                    |> Result.map (\m -> testTable m (doTest m))
                    |> resultToExpectation


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
            expectIntersection row col expectedText table
    in
        resultAndThen2 verify rowHeaderName colHeaderName


expectIntersection : String -> String -> String -> ElmHtml msg -> Result String Expectation
expectIntersection rowHeaderName colHeaderName text table =
    findIntersectionCellText rowHeaderName colHeaderName table
        |> Result.map (Expect.equal text)


expectIntersections : List ( String, String, String ) -> ElmHtml msg -> Expectation
expectIntersections expectations table =
    Expect.all (List.map (\( r, c, t ) -> \tab -> (expectIntersection r c t tab |> resultToExpectation)) expectations) table


findIntersectionCellText : String -> String -> ElmHtml msg -> Result String String
findIntersectionCellText rowHeaderName colHeaderName table =
    findIntersectionCell rowHeaderName colHeaderName table
        |> Result.andThen findTextFieldInCell
        |> Result.map HtmlTestExtra.getAttributes
        |> Result.map (getStringAttribute "value")
        |> Result.map (Maybe.withDefault "")


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


clickButton : ElmHtml Main.Msg -> Result String Main.Msg
clickButton button =
    HtmlTestExtra.simulate Event.click button


showImportExportPanel : Main.Model -> Result String Main.Model
showImportExportPanel initialModel =
    render initialModel
        |> findShowHideModelButton
        |> Result.fromMaybe "show/hide model button not found"
        |> Result.andThen clickButton
        |> Result.map ((flip Main.update) initialModel)


getImportExportContent : ElmHtml msg -> Result String String
getImportExportContent html =
    findExportImportTextField html
        |> Result.fromMaybe "import/export textarea not found"
        |> Result.map HtmlTestExtra.getAttributes
        |> Result.map (getStringAttribute "value")
        |> Result.map (Maybe.withDefault "")


updateImportExportContent : String -> ElmHtml Main.Msg -> Result String Main.Msg
updateImportExportContent newContent html =
    findExportImportTextField html
        |> Result.fromMaybe "import/export textarea not found"
        |> Result.andThen (HtmlTestExtra.simulate (Event.input newContent))


importAndFindFeatureTable : String -> Main.Model -> Result String (ElmHtml Main.Msg)
importAndFindFeatureTable typedText initialModel =
    showImportExportPanel initialModel
        |> Result.map render
        |> Result.andThen (updateImportExportContent typedText)
        |> Result.map ((flip Main.update) initialModel)
        |> Result.map render
        |> Result.map findFeatureTableElmHtml
        |> Result.andThen (Result.fromMaybe "feature table not found in view after import")


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


verifyShownModelContainsTypedIntersection : String -> Result String String -> Result String Expectation
verifyShownModelContainsTypedIntersection expected maybeContent =
    let
        verify content =
            let
                encodedText =
                    JE.encode 0 (JE.string expected) |> String.dropLeft 1 |> String.dropRight 1
            in
                Expect.true ("expected import export panel to contain '" ++ encodedText ++ "' but it was '" ++ content ++ "'") (String.contains encodedText content)
    in
        Result.map verify maybeContent


expectNotError : Result String a -> Expectation
expectNotError r =
    case r of
        Result.Ok _ ->
            Expect.pass

        Result.Err msg ->
            Expect.fail msg

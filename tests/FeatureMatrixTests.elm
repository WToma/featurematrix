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
import TableViewHelpers exposing (columnHeaderNames, rowHeaderNames, findFeatureTableElmHtml, findIntersectionCell, findTextFieldInCell, findColumnHeaderByName, findRowHeaderByName, extractHideButtonFromHeaderCell)
import ControlPanelHelpers exposing (findShowHideModelButton, findExportImportTextField, findNewFeatureDescription, findNewFeatureName, findAddNewFeatureButton, findHiddenFeatureByName, findShowFeatureButton)
import Json.Encode as JE
import Tuple


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


addFeature : Test
addFeature =
    describe "New features can be added to the feature table"
        [ test "the feature table reflects the newly added feature" <|
            \() ->
                addFeatureAndFindFeatureTable "New Awesome Feature" "New Awesome Description" initialModel
                    |> Result.map Tuple.first
                    |> Result.map columnHeaderNames
                    |> Result.andThen (Result.fromMaybe "column headers not found")
                    |> Result.map (\h -> Expect.true "expected headers to contain the new feature name" (List.member "New Awesome Feature" h))
                    |> resultToExpectation
        , test "the intersection between a newly added feature and an existing feature can be edited" <|
            \() ->
                let
                    addResult =
                        addFeatureAndFindFeatureTable "New Awesome Feature" "New Awesome Description" initialModel

                    table =
                        Result.map Tuple.first addResult

                    model =
                        Result.map Tuple.second addResult
                in
                    table
                        |> Result.andThen (typeIntoIntersection "New Awesome Feature" "Import" "New Awesome Intersection")
                        |> Result.map2 (flip Main.update) model
                        |> Result.andThen (getIntersectionByName "New Awesome Feature" "Import")
                        |> Result.map (Expect.equal "New Awesome Intersection")
                        |> resultToExpectation
        , test "intersections between newly added features are reflected in the exported model" <|
            \() ->
                let
                    addedBothModel =
                        Result.Ok initialModel
                            |> Result.andThen (addFeatureAndUpdateModel "Awesome Feature 1" "Awesome Desctiption 1")
                            |> Result.andThen (addFeatureAndUpdateModel "Awesome Feature 2" "Awesome Description 2")

                    importExportContent =
                        addedBothModel
                            |> Result.map render
                            |> Result.map findFeatureTableElmHtml
                            |> Result.andThen (Result.fromMaybe "feature table not found after adding features")
                            |> Result.andThen (typeIntoIntersection "Awesome Feature 1" "Awesome Feature 2" "Everything is Awesome")
                            |> Result.map2 (flip Main.update) addedBothModel
                            |> Result.andThen showImportExportPanel
                            |> Result.map render
                            |> Result.andThen getImportExportContent
                in
                    importExportContent
                        |> Result.map (\c -> Expect.true "expected import-export content to contain the new intersection" (String.contains "Everything is Awesome" c))
                        |> resultToExpectation
        , Test.skip <|
            test "newly added feature are reflected in the exported model if no intersection is edited" <|
                \() ->
                    let
                        importExportContent =
                            Result.Ok initialModel
                                |> Result.andThen (addFeatureAndUpdateModel "Awesome Feature 1" "Awesome Desctiption 1")
                                |> Result.andThen showImportExportPanel
                                |> Result.map render
                                |> Result.andThen getImportExportContent
                    in
                        importExportContent
                            |> Result.map
                                (\c ->
                                    Expect.true
                                        "expected import-export model to contain the new feature's name and description"
                                        ((String.contains "Awesome Feature 1" c) && (String.contains "Awesome Description 1" c))
                                )
                            |> resultToExpectation
        ]


hideFeature : Test
hideFeature =
    describe "Features can be hidden from the feature table using a button"
        [ test "hidden features do not show up in the feature table" <|
            \() ->
                let
                    featureTableAfterHide =
                        Result.Ok initialModel
                            |> Result.andThen (hideFeatureFromView "Import")
                            |> Result.map render
                            |> Result.map findFeatureTableElmHtml
                            |> Result.andThen (Result.fromMaybe "feature table not found after hiding")

                    verifyHeaders hs =
                        Expect.false "expected the headers not to contain the hidden feature" (List.member "Import" hs)

                    columnHeadersEx =
                        featureTableAfterHide
                            |> Result.map columnHeaderNames
                            |> Result.andThen (Result.fromMaybe "column header names not found after hide")
                            |> Result.map verifyHeaders
                            |> resultToExpectation

                    rowHeadersEx =
                        featureTableAfterHide
                            |> Result.map rowHeaderNames
                            |> Result.andThen (Result.fromMaybe "row header names not found after hide")
                            |> Result.map verifyHeaders
                            |> resultToExpectation
                in
                    Expect.all [ \() -> columnHeadersEx, \() -> rowHeadersEx ] ()
        , test "the row / column header hide button fires the same message" <|
            \() ->
                let
                    featureTable =
                        render initialModel
                            |> findFeatureTableElmHtml
                            |> Result.fromMaybe "feature table not found"

                    msgOnRow =
                        Result.andThen (pressHideButtonOnRow "Export") featureTable

                    msgOnCol =
                        Result.andThen (pressHideButtonOnColumn "Export") featureTable
                in
                    Expect.all [ expectNotError, Expect.equal msgOnCol ] msgOnRow
        , test "intersections for hidden features are exported" <|
            \() ->
                hideFeatureFromView "Edit Intersection" initialModel
                    -- initialModel has an intersection between this and Shows Features: "edited features are displayed"
                    |> Result.andThen showImportExportPanel
                    |> Result.map render
                    |> Result.andThen getImportExportContent
                    |> Result.map (\c -> Expect.true "the exported model should contain the hidden feature" (String.contains "edited features are displayed" c))
                    |> resultToExpectation
        , test "newly added features can be hidden" <|
            \() ->
                addFeatureAndFindFeatureTable "New Awesome Feature" "New Awesome Description" initialModel
                    |> Result.map Tuple.second
                    |> Result.andThen (hideFeatureFromView "New Awesome Feature")
                    |> Result.map render
                    |> Result.map findFeatureTableElmHtml
                    |> Result.andThen (Result.fromMaybe "feature table not found after hiding")
                    |> Result.map columnHeaderNames
                    |> Result.andThen (Result.fromMaybe "column header names not found after hide")
                    |> Result.map (\c -> Expect.false "column headers should not contain the hidden feature" (List.member "New Awesome Feature" c))
                    |> resultToExpectation
        , test "hidden features show up in the side bar, and they have a button that causes the feature to be shown again in the table along with its intersections which can be edited again" <|
            \() ->
                let
                    hiddenAndShownModel =
                        hideFeatureFromView "Edit Intersection" initialModel
                            -- initialModel has an intersection between this and Shows Features: "edited features are displayed"
                            |> Result.andThen (showFeature "Edit Intersection")

                    hiddenAndShownTable =
                        hiddenAndShownModel
                            |> Result.map render
                            |> Result.map findFeatureTableElmHtml
                            |> Result.andThen (Result.fromMaybe "feature table not found after hide & show")

                    headerNamesEx =
                        hiddenAndShownTable
                            |> Result.map columnHeaderNames
                            |> Result.andThen (Result.fromMaybe "column header names not found after hide & show")
                            |> Result.map (\c -> Expect.true "column headers include the hidden & shown feature name" (List.member "Edit Intersection" c))
                            |> resultToExpectation

                    intersectionEx =
                        hiddenAndShownTable
                            |> Result.andThen (findIntersectionCellText "Edit Intersection" "Shows Features")
                            |> Result.map (\c -> Expect.true "intersection displays the correct text after hide & show" (c == "edited features are displayed"))
                            |> resultToExpectation

                    editIntersectionEx =
                        hiddenAndShownTable
                            |> Result.andThen (typeIntoIntersection "Edit Intersection" "Shows Features" "updated the hidden and shown intersection")
                            |> Result.map2 (flip Main.update) hiddenAndShownModel
                            |> Result.map (verifyIntersectionText "edit" "showFeatures" "updated the hidden and shown intersection")
                            |> resultToExpectation
                in
                    Expect.all [ \() -> headerNamesEx, \() -> intersectionEx, \() -> editIntersectionEx ] ()
        , Test.skip <|
            test "import/export preserves the shown/hidden status of a feature" <|
                \() ->
                    let
                        hiddenExportContent =
                            hideFeatureFromView "Edit Intersection" initialModel
                                |> Result.andThen showImportExportPanel
                                |> Result.map render
                                |> Result.andThen getImportExportContent
                    in
                        importAndUpdateModel "{}" initialModel
                            |> resultAndThen2 importAndFindFeatureTable hiddenExportContent
                            |> Result.map columnHeaderNames
                            |> Result.andThen (Result.fromMaybe "column headers not found")
                            |> Result.map (\h -> Expect.false "expected headers not to include the hidden feature" (List.member "Edit Intersection" h))
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


importAndUpdateModel : String -> Main.Model -> Result String Main.Model
importAndUpdateModel typedText initialModel =
    showImportExportPanel initialModel
        |> Result.map render
        |> Result.andThen (updateImportExportContent typedText)
        |> Result.map ((flip Main.update) initialModel)


importAndFindFeatureTable : String -> Main.Model -> Result String (ElmHtml Main.Msg)
importAndFindFeatureTable typedText initialModel =
    importAndUpdateModel typedText initialModel
        |> Result.map render
        |> Result.map findFeatureTableElmHtml
        |> Result.andThen (Result.fromMaybe "feature table not found in view after import")


addFeatureAndFindFeatureTable : String -> String -> Main.Model -> Result String ( ElmHtml Main.Msg, Main.Model )
addFeatureAndFindFeatureTable newFeatureName newFeatureDescription initialModel =
    let
        newModel =
            addFeatureAndUpdateModel newFeatureName newFeatureDescription initialModel
    in
        newModel
            |> Result.map render
            |> Result.map findFeatureTableElmHtml
            |> Result.andThen (Result.fromMaybe "feature table not found in view after import")
            |> Result.map2 (flip (,)) newModel


addFeatureAndUpdateModel : String -> String -> Main.Model -> Result String Main.Model
addFeatureAndUpdateModel newFeatureName newFeatureDescription initialModel =
    let
        chainedUpdateReducer : (ElmHtml Main.Msg -> Result String Main.Msg) -> Result String Main.Model -> Result String Main.Model
        chainedUpdateReducer msgGenerator model =
            let
                rendered =
                    Result.map render model

                msg =
                    Result.andThen msgGenerator rendered
            in
                Result.map2 Main.update msg model
    in
        List.foldl chainedUpdateReducer
            (Result.Ok initialModel)
            [ updateNewFeatureName newFeatureName
            , updateNewFeatureDescription newFeatureDescription
            , pressNewFeatureButton
            ]


updateNewFeatureName : String -> ElmHtml Main.Msg -> Result String Main.Msg
updateNewFeatureName newFeatureName html =
    findNewFeatureName html
        |> Result.fromMaybe "new feature name textfield not found"
        |> Result.andThen (HtmlTestExtra.simulate (Event.input newFeatureName))


updateNewFeatureDescription : String -> ElmHtml Main.Msg -> Result String Main.Msg
updateNewFeatureDescription newFeatureDescription html =
    findNewFeatureDescription html
        |> Result.fromMaybe "new feature description textfield not found"
        |> Result.andThen (HtmlTestExtra.simulate (Event.input newFeatureDescription))


pressNewFeatureButton : ElmHtml Main.Msg -> Result String Main.Msg
pressNewFeatureButton html =
    findAddNewFeatureButton html
        |> Result.fromMaybe "new feature button not found"
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


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


hideFeatureFromView : String -> Main.Model -> Result String Main.Model
hideFeatureFromView featureName model =
    model
        |> render
        |> findFeatureTableElmHtml
        |> Result.fromMaybe "feature table not found"
        |> Result.andThen (pressHideButtonOnRow featureName)
        |> Result.map ((flip Main.update) model)


pressHideButtonOnRow : String -> ElmHtml Main.Msg -> Result String Main.Msg
pressHideButtonOnRow featureName featureTable =
    findRowHeaderByName featureName featureTable
        |> Result.fromMaybe ("row header not found for " ++ featureName)
        |> Result.map extractHideButtonFromHeaderCell
        |> Result.andThen (Result.fromMaybe "hide button not found on row header")
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


pressHideButtonOnColumn : String -> ElmHtml Main.Msg -> Result String Main.Msg
pressHideButtonOnColumn featureName featureTable =
    findColumnHeaderByName featureName featureTable
        |> Result.fromMaybe ("column header not found for " ++ featureName)
        |> Result.map extractHideButtonFromHeaderCell
        |> Result.andThen (Result.fromMaybe "hide button not found on column header")
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


showFeature : String -> Main.Model -> Result String Main.Model
showFeature featureName model =
    model
        |> render
        |> pressShowFeatureButton featureName
        |> Result.map ((flip Main.update) model)


pressShowFeatureButton : String -> ElmHtml Main.Msg -> Result String Main.Msg
pressShowFeatureButton featureName html =
    findHiddenFeatureByName featureName html
        |> Result.fromMaybe ("hidden feature " ++ featureName ++ " not found")
        |> Result.map findShowFeatureButton
        |> Result.andThen (Result.fromMaybe ("show feature button not found for hidden feature " ++ featureName))
        |> Result.andThen (HtmlTestExtra.simulate Event.click)

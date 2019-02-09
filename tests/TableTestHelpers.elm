module TableTestHelpers exposing (..)

import Test
import Fuzz
import Main
import Model
import Msg
import Expect exposing (Expectation)
import Helpers exposing (resultAndThen2, orderTuple)
import ElmHtml.InternalTypes exposing (ElmHtml)
import ModelTestHelpers exposing (getFeature)
import TestHelpers exposing (resultToExpectation)
import TableViewHelpers exposing (findFeatureTableElmHtml, findIntersectionCell, findTextFieldInCell, findRowHeaderByName, findColumnHeaderByName, extractHideButtonFromHeaderCell)
import Test.Html.Event as Event
import HtmlTestExtra exposing (getStringAttribute)
import Dict


typeIntoIntersectionTest : String -> Model.Model -> (Model.Model -> Result String Model.Model) -> (String -> String -> String -> Model.Model -> Result String Expectation) -> Test.Test
typeIntoIntersectionTest title modelBeforePreamble preamble verification =
    Test.fuzz3 Fuzz.int Fuzz.int Fuzz.string title <|
        \rowRandom colRandom intersectionText ->
            let
                initialModel =
                    preamble modelBeforePreamble

                testRowAndCol : Model.Model -> Model.Feature -> Model.Feature -> ElmHtml Msg.Msg -> Result String Expectation
                testRowAndCol initialModel rowFeature colFeature table =
                    typeIntoIntersection rowFeature.displayName colFeature.displayName intersectionText table
                        |> Result.map ((flip Main.update) initialModel)
                        |> Result.andThen (verification rowFeature.featureId colFeature.featureId intersectionText)

                doTest : Model.Model -> ElmHtml Msg.Msg -> Result String Expectation
                doTest initialModel table =
                    resultAndThen2
                        (\r c -> testRowAndCol initialModel r c table)
                        (getFeature initialModel rowRandom)
                        (getFeature initialModel colRandom)
            in
                preamble modelBeforePreamble
                    |> Result.map (\m -> testTable m (doTest m))
                    |> resultToExpectation


testTable : Model.Model -> (ElmHtml Msg.Msg -> Result String Expectation) -> Expectation
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


typeIntoIntersection : String -> String -> String -> ElmHtml Msg.Msg -> Result String Msg.Msg
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


verifyAllIntersectionsInTable : Model.Model -> ElmHtml msg -> Expectation
verifyAllIntersectionsInTable model table =
    let
        featureIds =
            List.map .featureId model.persistent.features

        allIntersections =
            List.concatMap (\f1 -> List.map ((,) f1) featureIds) featureIds

        allExpectations : List (ElmHtml msg -> Expectation)
        allExpectations =
            List.map
                (\( f1, f2 ) -> \table -> resultToExpectation (verifyIntersectionTextInTable f1 f2 model table))
                allIntersections
    in
        Expect.all allExpectations table


verifyIntersectionTextInTable : String -> String -> Model.Model -> ElmHtml msg -> Result String Expectation
verifyIntersectionTextInTable rowFeatureId colFeatureId model table =
    let
        featureIdToFeatureMap =
            Dict.fromList <| List.map (\f -> ( f.featureId, f )) model.persistent.features

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
            Dict.get (orderTuple ( rowFeatureId, colFeatureId )) model.persistent.intersections
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


hideFeatureFromView : String -> Model.Model -> Result String Model.Model
hideFeatureFromView featureName model =
    model
        |> render
        |> findFeatureTableElmHtml
        |> Result.fromMaybe "feature table not found"
        |> Result.andThen (pressHideButtonOnRow featureName)
        |> Result.map ((flip Main.update) model)


pressHideButtonOnRow : String -> ElmHtml Msg.Msg -> Result String Msg.Msg
pressHideButtonOnRow featureName featureTable =
    findRowHeaderByName featureName featureTable
        |> Result.fromMaybe ("row header not found for " ++ featureName)
        |> Result.map extractHideButtonFromHeaderCell
        |> Result.andThen (Result.fromMaybe "hide button not found on row header")
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


pressHideButtonOnColumn : String -> ElmHtml Msg.Msg -> Result String Msg.Msg
pressHideButtonOnColumn featureName featureTable =
    findColumnHeaderByName featureName featureTable
        |> Result.fromMaybe ("column header not found for " ++ featureName)
        |> Result.map extractHideButtonFromHeaderCell
        |> Result.andThen (Result.fromMaybe "hide button not found on column header")
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


render : Model.Model -> ElmHtml Msg.Msg
render model =
    Main.view model |> HtmlTestExtra.fromHtml

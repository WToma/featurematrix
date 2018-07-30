module FeatureMatrixTests exposing (..)

import Test exposing (Test, describe, todo, test)
import Expect exposing (Expectation)
import Main
import Dict
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text, tag, class)
import ElmHtml.Query as ElmHtmlQuery
import HtmlTestExtra
import ElmHtml.InternalTypes exposing (ElmHtml)
import Set exposing (Set)
import Helpers exposing (..)
import Html exposing (Html)


suite : Test
suite =
    describe "There is Table Mode in which all feature intersections can be seen."
        [ test "all column and row headers are shown" <|
            \() ->
                let
                    expectations =
                        [ expectColumnHeaderNames, expectRowHeaderNames ]

                    expectDummyFeatureDisplayNames =
                        List.map (\e -> e dummyFeatureDisplayNames) expectations
                in
                    Expect.all expectDummyFeatureDisplayNames initialView
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


dummyFeatureDisplayNames : List String
dummyFeatureDisplayNames =
    List.map (\df -> df.displayName) Main.dummyFeatures


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
        html
            |> Query.findAll [ tag "tr" ]
            |> Query.first
            |> Query.has allColumnHeadersSelector


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

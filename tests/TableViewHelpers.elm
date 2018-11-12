module TableViewHelpers exposing (columnHeaderNames, rowHeaderNames, findFeatureTableElmHtml, findIntersectionCell, findTextFieldInCell)

import ElmHtml.InternalTypes exposing (ElmHtml)
import ElmHtml.Query exposing (queryByTagName, queryByClassName)
import Helpers exposing (..)
import HtmlTestExtra


{-| Given the feature table returns the column header names in the order they appear in the table.
-}
columnHeaderNames : ElmHtml msg -> Maybe (List String)
columnHeaderNames featureTable =
    let
        findColumnHeaderRow : ElmHtml msg -> Maybe (ElmHtml msg)
        findColumnHeaderRow html =
            html
                |> queryByTagName "tr"
                |> List.head

        -- drop the first cell which does not contain a header
        columnHeaderCells : ElmHtml msg -> Maybe (List (ElmHtml msg))
        columnHeaderCells headerRow =
            List.tail <| queryByTagName "th" headerRow
    in
        findColumnHeaderRow featureTable
            |> Maybe.andThen columnHeaderCells
            |> Maybe.map (List.map extractHeaderCellText)
            |> Maybe.map flattenMaybeList


{-| Given the feature table, returns the row header names in the order they appear in the table.
-}
rowHeaderNames : ElmHtml msg -> Maybe (List String)
rowHeaderNames table =
    let
        getRowHeader row =
            row
                |> queryByTagName "th"
                |> List.head
                |> Maybe.andThen extractHeaderCellText

        rowHeaders =
            table
                |> queryByTagName "tr"
                |> List.tail
                -- ignore the first row, which contains the column headers
                |> Maybe.map (List.map getRowHeader)
    in
        rowHeaders
            |> Maybe.map flattenMaybeList


extractHeaderCellText : ElmHtml msg -> Maybe String
extractHeaderCellText th =
    queryByTagName "div" th
        |> List.head
        |> Maybe.map HtmlTestExtra.extractText
        |> Maybe.andThen ensureSingleton


{-| Given the view result, returns the feature table element.
-}
findFeatureTableElmHtml : ElmHtml msg -> Maybe (ElmHtml msg)
findFeatureTableElmHtml html =
    let
        results =
            html
                -- same note as in findFeatureTable: the order of selectors matters a lot unfortunately
                |> queryByClassName "featureTable"
                |> List.concatMap (\elemWithClassName -> queryByTagName "table" elemWithClassName)
    in
        ensureSingleton results


{-| Given the label for the row, the column, and a feature table it returns the cell in the intersection of the
given row and column, or an error message if it doesn't exist.
-}
findIntersectionCell : String -> String -> ElmHtml msg -> Result String (ElmHtml msg)
findIntersectionCell rowLabel colLabel featureTable =
    let
        headerIndex labelToFind findHeaderLabels typeInError =
            findHeaderLabels featureTable
                |> Maybe.andThen (firstIndexOf <| (==) labelToFind)
                |> Result.fromMaybe (typeInError ++ " with label '" ++ labelToFind ++ "' not found")

        rowHeaderIndex =
            headerIndex rowLabel rowHeaderNames "row" |> Result.map ((+) 1)

        row =
            Result.andThen
                (\rhi ->
                    (listElemAtIndexResult ("table did not contain enough rows"))
                        rhi
                        (queryByTagName "tr" featureTable)
                )
                rowHeaderIndex

        columnHeaderIndex =
            headerIndex colLabel columnHeaderNames "column"

        cell =
            resultAndThen2
                (listElemAtIndexResult ("row with label '" ++ rowLabel ++ "' does not have enough cells"))
                columnHeaderIndex
                (Result.map (queryByTagName "td") row)
    in
        cell


findTextFieldInCell : ElmHtml msg -> Result String (ElmHtml msg)
findTextFieldInCell intersectionCell =
    intersectionCell
        |> queryByTagName "textarea"
        |> ensureSingleton
        |> Result.fromMaybe "could not find unique text area"

module FeatureMatrixTestFramework
    exposing
        ( SelectionPathElement
        , Selector
        , Operation
        , Verification
        , initialState
        , select
        , operate
        , verify
        )

import Main
import Msg
import ElmHtml.InternalTypes exposing (ElmHtml)
import HtmlTestExtra
import Expect
import TestHelpers


-- exported types


type alias SelectionPathElement =
    { name : String
    , selectionSteps : List String
    }


type alias Selector =
    { selectionName : SelectionPathElement
    , select : ElmHtml Msg.Msg -> Maybe (ElmHtml Msg.Msg)
    }


type alias Operation =
    { description : String
    , operate : ElmHtml Msg.Msg -> Maybe Msg.Msg
    }


type alias Verification =
    { description : String
    , verify : ElmHtml Msg.Msg -> Maybe Expect.Expectation
    }



-- unexported types


type alias FMApplication =
    { model : Main.Model
    , rendered : ElmHtml Msg.Msg
    , selected : ElmHtml Msg.Msg
    , selectionPath : List SelectionPathElement -- newest first
    , operationDescriptions : List String -- newest first
    }


type Failure
    = SelectionNotFound (List SelectionPathElement)
    | OperationNotFound (List SelectionPathElement) String
    | VerificationError (List SelectionPathElement) String


type alias TestState =
    Result Failure FMApplication



-- exported functions


initialState : Main.Model -> TestState
initialState model =
    let
        rendered =
            Main.view model |> HtmlTestExtra.fromHtml
    in
        Ok
            { model = model
            , rendered = rendered
            , selected = rendered
            , selectionPath = []
            , operationDescriptions = []
            }


select : Selector -> TestState -> TestState
select selector tstst =
    Result.andThen (selectFromApplication selector) tstst


operate : Operation -> TestState -> TestState
operate operation tstst =
    Result.andThen (operateOnApplication operation) tstst


verify : Verification -> TestState -> Expect.Expectation
verify verification tstst =
    tstst
        |> Result.andThen (verifyOnApplication verification)
        |> Result.mapError formatFailure
        |> TestHelpers.resultToExpectation



-- unexported


advanceApplicationByMsg : Msg.Msg -> String -> FMApplication -> FMApplication
advanceApplicationByMsg msg operationDescription application =
    let
        newModel =
            Main.update msg application.model

        newOpDescriptions =
            operationDescription :: application.operationDescriptions

        newRendered =
            Main.view newModel |> HtmlTestExtra.fromHtml
    in
        { model = newModel
        , rendered = newRendered
        , selected = newRendered
        , selectionPath = []
        , operationDescriptions = newOpDescriptions
        }


selectFromApplication : Selector -> FMApplication -> TestState
selectFromApplication selector application =
    let
        fullSelectionPath =
            selector.selectionName :: application.selectionPath
    in
        case selector.select application.rendered of
            Just selected ->
                Ok { application | selected = selected, selectionPath = fullSelectionPath }

            Nothing ->
                Err (SelectionNotFound fullSelectionPath)


operateOnApplication : Operation -> FMApplication -> TestState
operateOnApplication operation application =
    case operation.operate application.selected of
        Just msg ->
            Ok <| advanceApplicationByMsg msg operation.description application

        Nothing ->
            Err (OperationNotFound application.selectionPath operation.description)


verifyOnApplication : Verification -> FMApplication -> Result Failure Expect.Expectation
verifyOnApplication verification application =
    verification.verify application.selected
        |> Result.fromMaybe (VerificationError application.selectionPath verification.description)


formatFailure : Failure -> String
formatFailure failure =
    case failure of
        SelectionNotFound reversePath ->
            let
                path =
                    List.reverse reversePath

                pathElNames =
                    List.map .name path

                pathFailed =
                    List.head pathElNames |> Maybe.withDefault ""

                remainingPath =
                    List.tail pathElNames |> Maybe.withDefault []

                remainingPathDescription =
                    String.join " -> " remainingPath
            in
                "selection '" ++ pathFailed ++ "' was not found afer the following selections: " ++ remainingPathDescription

        OperationNotFound reversePath opNotFound ->
            let
                pathDescription =
                    List.reverse reversePath
                        |> List.map .name
                        |> String.join " -> "
            in
                "operation '" ++ opNotFound ++ "' was not found at the following selection: " ++ pathDescription

        VerificationError reversePath verificationDescription ->
            let
                pathDescription =
                    List.reverse reversePath
                        |> List.map .name
                        |> String.join " -> "
            in
                "verification failed: " ++ verificationDescription ++ " at path " ++ pathDescription

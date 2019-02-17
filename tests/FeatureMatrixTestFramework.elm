module FeatureMatrixTestFramework
    exposing
        ( SelectionPathElement
        , Selector
        , Operation
        , Verification
        , TestState
        , initialState
        , select
        , operate
        , verify
        , snapshot
        , verifySnapshot
        , defaultSelector
        )

import Main
import Msg
import ElmHtml.InternalTypes exposing (ElmHtml)
import HtmlTestExtra
import Expect
import TestHelpers
import Dict exposing (Dict)


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


type FMApplication
    = FMApplication
        { model : Main.Model
        , rendered : ElmHtml Msg.Msg
        , selected : ElmHtml Msg.Msg
        , selectionPath : List SelectionPathElement -- newest first
        , operationDescriptions : List String -- newest first
        , snapshots : Dict String FMApplication
        , defaultSelector : Maybe Selector
        }


type Failure
    = SelectionNotFound (List SelectionPathElement)
    | OperationNotFound (List SelectionPathElement) String
    | VerificationError (List SelectionPathElement) String
    | SnapshotAlreadyExists String
    | SnapshotDoesNotExist String


type alias TestState =
    Result Failure FMApplication



-- exported functions


initialState : Main.Model -> TestState
initialState model =
    let
        rendered =
            Main.view model |> HtmlTestExtra.fromHtml
    in
        Ok <|
            FMApplication
                { model = model
                , rendered = rendered
                , selected = rendered
                , selectionPath = []
                , operationDescriptions = []
                , snapshots = Dict.empty
                , defaultSelector = Nothing
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


snapshot : String -> TestState -> TestState
snapshot snapshotName tstst =
    Result.andThen (snapshotApplication snapshotName) tstst


verifySnapshot : String -> Verification -> TestState -> Expect.Expectation
verifySnapshot snapshotName verification tstst =
    tstst
        |> Result.andThen (verifySnapshotOnApplication snapshotName verification)
        |> Result.mapError formatFailure
        |> TestHelpers.resultToExpectation


defaultSelector : Maybe Selector -> TestState -> TestState
defaultSelector newDefaultSelector tstst =
    Result.map (setDefaultSelectorOnApplication newDefaultSelector) tstst



-- unexported


advanceApplicationByMsg : Msg.Msg -> String -> FMApplication -> FMApplication
advanceApplicationByMsg msg operationDescription (FMApplication application) =
    let
        newModel =
            Main.update msg application.model

        newOpDescriptions =
            operationDescription :: application.operationDescriptions

        newRendered =
            Main.view newModel |> HtmlTestExtra.fromHtml
    in
        FMApplication
            { application
                | model = newModel
                , rendered = newRendered
                , selected = newRendered
                , selectionPath = []
                , operationDescriptions = newOpDescriptions
            }


selectFromApplication : Selector -> FMApplication -> TestState
selectFromApplication selector (FMApplication application) =
    let
        fullSelectionPath =
            selector.selectionName :: application.selectionPath
    in
        case selector.select application.rendered of
            Just selected ->
                Ok <| FMApplication { application | selected = selected, selectionPath = fullSelectionPath }

            Nothing ->
                Err (SelectionNotFound fullSelectionPath)


operateOnApplication : Operation -> FMApplication -> TestState
operateOnApplication operation ((FMApplication application) as fmApplication) =
    case operation.operate application.selected of
        Just msg ->
            let
                updatedApplication =
                    advanceApplicationByMsg msg operation.description fmApplication
            in
                case application.defaultSelector of
                    Nothing ->
                        Ok updatedApplication

                    Just defaultSelector ->
                        selectFromApplication defaultSelector updatedApplication

        Nothing ->
            Err (OperationNotFound application.selectionPath operation.description)


verifyOnApplication : Verification -> FMApplication -> Result Failure Expect.Expectation
verifyOnApplication verification (FMApplication application) =
    verification.verify application.selected
        |> Result.fromMaybe (VerificationError application.selectionPath verification.description)


snapshotApplication : String -> FMApplication -> TestState
snapshotApplication snapshotName ((FMApplication application) as fmApplication) =
    if not (Dict.member snapshotName application.snapshots) then
        Ok <|
            FMApplication
                { application
                    | snapshots = Dict.insert snapshotName fmApplication application.snapshots
                }
    else
        Err <| (SnapshotAlreadyExists snapshotName)


verifySnapshotOnApplication : String -> Verification -> FMApplication -> Result Failure Expect.Expectation
verifySnapshotOnApplication snapshotName verification ((FMApplication application) as fmApplication) =
    Dict.get snapshotName application.snapshots
        |> Result.fromMaybe (SnapshotDoesNotExist snapshotName)
        |> Result.andThen (verifyOnApplication verification)


setDefaultSelectorOnApplication : Maybe Selector -> FMApplication -> FMApplication
setDefaultSelectorOnApplication newDefaultSelector (FMApplication application) =
    FMApplication { application | defaultSelector = newDefaultSelector }


formatFailure : Failure -> String
formatFailure failure =
    case failure of
        SelectionNotFound reversePath ->
            let
                pathFailed =
                    List.head reversePath |> Maybe.map .name |> Maybe.withDefault ""

                remainingPath =
                    List.tail reversePath |> Maybe.withDefault []
            in
                "selection '" ++ pathFailed ++ "' was not found afer the following selections: " ++ (describePath remainingPath)

        OperationNotFound reversePath opNotFound ->
            "operation '" ++ opNotFound ++ "' was not found at the following selection: " ++ (describePath reversePath)

        VerificationError reversePath verificationDescription ->
            "verification failed: " ++ verificationDescription ++ " at path " ++ (describePath reversePath)

        SnapshotAlreadyExists snapshotName ->
            "attempted to take snapshot '" ++ snapshotName ++ "' twice"

        SnapshotDoesNotExist snapshotName ->
            "attempted to access snapshot '" ++ snapshotName ++ "' which does not exist"


describePath : List SelectionPathElement -> String
describePath reversePath =
    if reversePath == [] then
        "App.view"
    else
        List.reverse reversePath
            |> List.map .name
            |> String.join " -> "

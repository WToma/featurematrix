module FeatureMatrixTestFramework
    exposing
        ( SelectionStepName
        , SelectionName
        , ElementarySelectionStep
        , Selector
        , Operation
        , Verification
        , TestState
        , initialState
        , identitySelector
        , select
        , operate
        , verify
        , snapshot
        , verifySnapshot
        , defaultSelector
        , buildSelector
        , combineSelectors
        )

import Main
import Msg
import ElmHtml.InternalTypes exposing (ElmHtml)
import HtmlTestExtra
import Expect
import TestHelpers
import Dict exposing (Dict)


-- exported types


type alias SelectionStepName =
    String


type alias SelectionName =
    String


type alias FailedSelection =
    { failedSelectionStep : SelectionStepName
    , successfulSelectionSteps : List SelectionStepName
    }


type alias ElementarySelectionStep =
    ( SelectionStepName, ElmHtml Msg.Msg -> Maybe (ElmHtml Msg.Msg) )


type alias Selector =
    { selectionName : SelectionName
    , select : ElmHtml Msg.Msg -> Result FailedSelection (ElmHtml Msg.Msg)
    }


type alias Operation =
    { description : String
    , select : Selector
    , operate : ElmHtml Msg.Msg -> Result String Msg.Msg
    }


type alias Verification =
    { description : String
    , select : Selector
    , verify : ElmHtml Msg.Msg -> Result String Expect.Expectation
    }



-- unexported types


type FMApplication
    = FMApplication
        { model : Main.Model
        , rendered : ElmHtml Msg.Msg
        , selected : ElmHtml Msg.Msg
        , selectionPath : List SelectionName -- newest first
        , operationDescriptions : List String -- newest first
        , snapshots : Dict String FMApplication
        , defaultSelector : Maybe Selector
        }


type alias FailedApplicationSelection =
    { successfulSelections : List SelectionName
    , failedSelectionName : String
    , failedSelection : FailedSelection
    }


type Failure
    = SelectionNotFound FailedApplicationSelection
    | OperationSelectionNotFound FailedApplicationSelection String
    | OperationFailed
        { successfulSelections : List SelectionName
        , operationDescription : String
        , operationFailure : String
        }
    | VerificationSelectionNotFound FailedApplicationSelection String
    | VerificationError
        { successfulSelections : List SelectionName
        , verificationDescription : String
        , verificationFailure : String
        }
    | SnapshotAlreadyExists String
    | SnapshotDoesNotExist String


type alias TestState =
    Result Failure FMApplication


type alias PartialSelector =
    ElmHtml Msg.Msg -> Result FailedSelection ( List String, ElmHtml Msg.Msg )



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


identitySelector : Selector
identitySelector =
    { selectionName = identitySelectorName, select = Ok }


select : Selector -> TestState -> TestState
select selector tstst =
    tstst
        |> Result.map (selectFromApplication selector)
        |> Result.andThen (Result.mapError SelectionNotFound)


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


buildSelector : SelectionName -> List ElementarySelectionStep -> Selector
buildSelector name steps =
    let
        select : PartialSelector
        select =
            steps |> List.foldl appendBasicSelector (\html -> Ok ( [], html ))
    in
        { selectionName = name
        , select = (select >> (Result.map Tuple.second))
        }


{-| (>>) for selectors
-}
combineSelectors : Selector -> Selector -> Selector
combineSelectors selector1 selector2 =
    { selectionName = selector2.selectionName ++ " in " ++ selector1.selectionName
    , select =
        selector1.select
            >> Result.andThen
                (selector2.select
                    >> Result.mapError
                        (\failedSelection2 ->
                            if selector1.selectionName /= identitySelectorName then
                                { failedSelection2
                                    | successfulSelectionSteps = failedSelection2.successfulSelectionSteps ++ [ selector1.selectionName ]
                                }
                            else
                                failedSelection2
                        )
                )
    }



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


selectFromApplication : Selector -> FMApplication -> Result FailedApplicationSelection FMApplication
selectFromApplication selector (FMApplication application) =
    case selector.select application.selected of
        Ok selected ->
            Ok <|
                FMApplication
                    { application
                        | selected = selected
                        , selectionPath = selector.selectionName :: application.selectionPath
                    }

        Err failedSelection ->
            Err
                { successfulSelections = application.selectionPath
                , failedSelectionName = selector.selectionName
                , failedSelection = failedSelection
                }


operateOnApplication : Operation -> FMApplication -> TestState
operateOnApplication operation ((FMApplication application) as fmApplication) =
    case selectFromApplication operation.select fmApplication of
        Ok ((FMApplication application) as fmApplication) ->
            case operation.operate application.selected of
                Ok msg ->
                    let
                        updatedApplication =
                            advanceApplicationByMsg msg operation.description fmApplication
                    in
                        case application.defaultSelector of
                            Nothing ->
                                Ok updatedApplication

                            Just defaultSelector ->
                                selectFromApplication defaultSelector updatedApplication
                                    |> Result.mapError SelectionNotFound

                Err operationFailure ->
                    Err <|
                        OperationFailed
                            { successfulSelections = application.selectionPath
                            , operationDescription = operation.description
                            , operationFailure = operationFailure
                            }

        Err failedApplicationSelection ->
            Err (OperationSelectionNotFound failedApplicationSelection operation.description)


verifyOnApplication : Verification -> FMApplication -> Result Failure Expect.Expectation
verifyOnApplication verification ((FMApplication application) as fmApplication) =
    case selectFromApplication verification.select fmApplication of
        Ok (FMApplication application) ->
            verification.verify application.selected
                |> Result.mapError
                    (\failure ->
                        VerificationError
                            { successfulSelections = application.selectionPath
                            , verificationDescription = verification.description
                            , verificationFailure = failure
                            }
                    )

        Err failedApplicationSelection ->
            Err <| VerificationSelectionNotFound failedApplicationSelection verification.description


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
        SelectionNotFound failedApplicationSelection ->
            describeFailedApplicationSelection failedApplicationSelection

        OperationSelectionNotFound failedSelection operationDescription ->
            "operation '"
                ++ operationDescription
                ++ "' failed because "
                ++ (describeFailedApplicationSelection failedSelection)

        OperationFailed { successfulSelections, operationDescription, operationFailure } ->
            "operation '"
                ++ operationDescription
                ++ "' failed at the following selection: "
                ++ (describePath successfulSelections)
                ++ ". the details of the failed operation:\n"
                ++ operationFailure

        VerificationSelectionNotFound failedSelection verificationDescription ->
            "verification '"
                ++ verificationDescription
                ++ "' failed because "
                ++ (describeFailedApplicationSelection failedSelection)

        VerificationError { successfulSelections, verificationDescription, verificationFailure } ->
            "verification ' "
                ++ verificationDescription
                ++ "' failed at the following selection "
                ++ (describePath successfulSelections)
                ++ ". the details of the failed verification:\n"
                ++ verificationFailure

        SnapshotAlreadyExists snapshotName ->
            "attempted to take snapshot '" ++ snapshotName ++ "' twice"

        SnapshotDoesNotExist snapshotName ->
            "attempted to access snapshot '" ++ snapshotName ++ "' which does not exist"


describeFailedApplicationSelection : FailedApplicationSelection -> String
describeFailedApplicationSelection { successfulSelections, failedSelectionName, failedSelection } =
    "selection '"
        ++ failedSelectionName
        ++ "' was not found afer the following selections: "
        ++ (describePath successfulSelections)
        ++ ". the details of the failed selection:\n"
        ++ (describeFailedSelection failedSelection)


identitySelectorName : String
identitySelectorName =
    "FeatureMatrixTestFrameworkInternal__identity"


describePath : List SelectionName -> String
describePath reversePath =
    let
        filteredReversePath =
            List.filter (\x -> x /= identitySelectorName) reversePath
    in
        if filteredReversePath == [] then
            "App.view"
        else
            List.reverse filteredReversePath
                |> String.join " -> "


describeFailedSelection : FailedSelection -> String
describeFailedSelection failedSelection =
    let
        separator =
            " ->\n"

        okElements =
            failedSelection.successfulSelectionSteps
                |> List.reverse
                |> List.map (\e -> e ++ " ok")
                |> String.join separator

        okElementsAndSeparator =
            if failedSelection.successfulSelectionSteps /= [] then
                okElements ++ separator
            else
                ""
    in
        okElementsAndSeparator ++ failedSelection.failedSelectionStep ++ " <-- THIS ONE FAILED"


appendBasicSelector : ElementarySelectionStep -> PartialSelector -> PartialSelector
appendBasicSelector ( newSelectionName, selectFurther ) selector =
    \html ->
        case selector html of
            Ok ( successfulPath, partialSelected ) ->
                case selectFurther partialSelected of
                    Just selected ->
                        Ok ( newSelectionName :: successfulPath, selected )

                    Nothing ->
                        Err { failedSelectionStep = newSelectionName, successfulSelectionSteps = successfulPath }

            Err failedAlready ->
                Err failedAlready

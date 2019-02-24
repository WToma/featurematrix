module FocusModeTests exposing (..)

import Test exposing (Test, describe, todo, test)
import FeatureMatrixTestFramework exposing (..)
import TableViewHelpers exposing (findFeatureTableElmHtml, findColumnHeaderByName, extractFocusButtonFromHeaderCell, findIntersectionCell)
import ControlPanelHelpers
import HtmlTestExtra exposing (..)
import ElmHtml.Query exposing (queryByClassName, queryByTagName)
import ElmHtml.InternalTypes exposing (ElmHtml)
import Test.Html.Event as Event
import Helpers exposing (ensureSingleton)
import Expect
import TestHelpers exposing (initialModel, initialFirstFeatureName, initialSecondFeatureName)


entering : Test
entering =
    describe "To enter focus mode"
        [ test "there is a button on the feature headers in table view" <|
            \() ->
                (initialState initialModel)
                    |> select featureTable
                    |> select (columnHeader "Import")
                    |> operate enterFocusMode
                    |> select focusModePanel
                    |> verify (focusedFeature "Import")
        , test "there is a button on the add feature panel" <|
            \() ->
                (initialState initialModel)
                    |> operate (typeNewFeatureName "New Feature")
                    |> operate (typeNewFeatureDescription "New Description")
                    |> operate pressNewFeatureFocusButton
                    |> select focusModePanel
                    |> verify (focusedFeature "New Feature")
        ]


focusMode : Test
focusMode =
    describe "In Focus Mode"
        [ test "there is no feature table" <|
            \() ->
                (initialState initialModel)
                    |> focusOn "Import"
                    |> verify noFeatureTableShown
        , test "there are buttons to go to the previous / next feature, the other feature for the intersection is fixed" <|
            \() ->
                (initialState initialModel)
                    |> defaultSelector (Just focusModePanel)
                    |> focusOn "Import"
                    |> snapshot "before"
                    |> operate pressNextFeatureButton
                    |> snapshot "after"
                    |> Expect.all
                        [ verifySnapshot "before" (crossFeature initialFirstFeatureName)
                        , verifySnapshot "after" (crossFeature initialSecondFeatureName)
                        , verifySnapshot "after" (focusedFeature "Import")
                        ]
        , test "one intersection is shown at a time" <|
            \() ->
                (initialState initialModel)
                    |> select featureTable
                    |> select (tableIntersection "Import" initialFirstFeatureName)
                    |> operate (typeIntoTextarea "First Awesome Content")
                    |> select featureTable
                    |> select (tableIntersection "Import" initialSecondFeatureName)
                    |> operate (typeIntoTextarea "Second Awesome Content")
                    |> defaultSelector (Just focusModePanel)
                    |> focusOn "Import"
                    |> snapshot "first focus"
                    |> operate pressNextFeatureButton
                    |> snapshot "second focus"
                    |> Expect.all
                        [ verifySnapshot "first focus" (focusIntersectionContent "First Awesome Content")
                        , verifySnapshot "second focus" (focusIntersectionContent "Second Awesome Content")
                        ]
        , test "that intersection can be edited" <|
            \() ->
                (initialState initialModel)
                    |> defaultSelector (Just focusModePanel)
                    |> focusOn "Import"
                    |> select focusIntersection
                    |> operate (typeIntoTextarea "Awesome content that was edited in focus mode")
                    |> operate pressNextFeatureButton
                    |> operate pressPreviousFeatureButton
                    |> verify (focusIntersectionContent "Awesome content that was edited in focus mode")
        , test "there is a button to return to table view; intersections edited in focus mode are preserved in table view" <|
            \() ->
                (initialState initialModel)
                    |> focusOn "Import"
                    |> select focusModePanel
                    |> select focusIntersection
                    |> operate (typeIntoTextarea "Awesome content edited in focus mode")
                    |> select focusModePanel
                    |> operate pressReturnToTableViewButton
                    |> select featureTable
                    |> verify (tableIntersectionContent "Import" initialFirstFeatureName "Awesome content edited in focus mode")
        , todo "export works the same way as in table view"
        , todo "if a new feature is added, the new feature can be reached while cycling through the features"
        , todo "hidden features still show up"
        ]


importing : Test
importing =
    describe "When importing in Focus Mode"
        [ todo "if the import changes the currently focused intersection, it's shown immediately"
        , todo "if the focused feature no longer exists, the view returns to table view"
        , todo "if the other feature currently shown no longer exists, we go to the next feature that exists"
        , todo "if the import text is invalid, an error message is shown (the same as in table view)"
        ]



-- selectors


featureTable : Selector
featureTable =
    buildSelector "Feature Table" [ ( "<table class='featureTable'>", TableViewHelpers.findFeatureTableElmHtml ) ]


columnHeader : String -> Selector
columnHeader featureDisplayName =
    buildSelector
        ("Column Header called " ++ featureDisplayName)
        [ ( "<div>" ++ featureDisplayName ++ "</div>", TableViewHelpers.findColumnHeaderByName featureDisplayName ) ]


focusModePanel : Selector
focusModePanel =
    buildSelector "Focus Mode" [ esClassName "focusModeWrapper" ]


tableIntersection : String -> String -> Selector
tableIntersection rowFeatureDisplayName colFeatureDisplayName =
    { selectionName = "The intersection of '" ++ rowFeatureDisplayName ++ "' and '" ++ colFeatureDisplayName ++ "'"
    , select =
        TableViewHelpers.findIntersectionCell rowFeatureDisplayName colFeatureDisplayName
            >> Result.mapError (\err -> { failedSelectionStep = err, successfulSelectionSteps = [] })
    }


focusIntersection : Selector
focusIntersection =
    buildSelector "the intersection area of the focus panel" [ esClassName "intersection" ]



-- operations


enterFocusMode : Operation
enterFocusMode =
    { description = "press the button with the focusBtn class"
    , operate = TableViewHelpers.extractFocusButtonFromHeaderCell >> Maybe.andThen clickButton
    }


typeNewFeatureName : String -> Operation
typeNewFeatureName newFeatureName =
    { description = "type '" ++ newFeatureName ++ "' into the new feature name input"
    , operate = ControlPanelHelpers.updateNewFeatureName newFeatureName >> Result.toMaybe
    }


typeNewFeatureDescription : String -> Operation
typeNewFeatureDescription newFeatureDescription =
    { description = "type '" ++ newFeatureDescription ++ "' into the new feature description input"
    , operate = ControlPanelHelpers.updateNewFeatureDescription newFeatureDescription >> Result.toMaybe
    }


pressNewFeatureFocusButton : Operation
pressNewFeatureFocusButton =
    { description = "press the button with the addFeatureAndFocus class"
    , operate = queryByClassName "addFeatureAndFocus" >> ensureSingleton >> Maybe.andThen clickButton
    }


pressNextFeatureButton : Operation
pressNextFeatureButton =
    { description = "press the button with the nextFeature class"
    , operate = queryByClassName "nextFeature" >> ensureSingleton >> Maybe.andThen clickButton
    }


pressPreviousFeatureButton : Operation
pressPreviousFeatureButton =
    { description = "press the button with the previousFeature class"
    , operate = queryByClassName "previousFeature" >> ensureSingleton >> Maybe.andThen clickButton
    }


typeIntoTextarea : String -> Operation
typeIntoTextarea content =
    { description = "type '" ++ content ++ "' into the only textarea of the current selection"
    , operate = queryByTagName "textarea" >> ensureSingleton >> Maybe.andThen (typeInto content)
    }


pressReturnToTableViewButton : Operation
pressReturnToTableViewButton =
    { description = "press the button with the returnToTableView class"
    , operate = queryByClassName "returnToTableView" >> ensureSingleton >> Maybe.andThen clickButton
    }



-- complex selection / operation shortcuts


focusOn : String -> TestState -> TestState
focusOn featureName =
    select featureTable >> select (columnHeader featureName) >> operate enterFocusMode



-- verifications


focusedFeature : String -> Verification
focusedFeature expectedFocusedFeatureName =
    { description = "the content of the element with the focusedFeature class should be " ++ expectedFocusedFeatureName
    , verify =
        \focusModeWrapper ->
            focusModeWrapper
                |> (queryByClassName "focusedFeature" >> ensureSingleton)
                |> Maybe.andThen featureCardGetFeatureName
                |> Maybe.map (Expect.equal expectedFocusedFeatureName)
    }


crossFeature : String -> Verification
crossFeature expectedCrossFeatureName =
    { description = "the content of the element with the crossFeature class should be " ++ expectedCrossFeatureName
    , verify =
        \focusModeWrapper ->
            focusModeWrapper
                |> (queryByClassName "crossFeature" >> ensureSingleton)
                |> Maybe.andThen featureCardGetFeatureName
                |> Maybe.map (Expect.equal expectedCrossFeatureName)
    }


noFeatureTableShown : Verification
noFeatureTableShown =
    { description = "no feature table is shown"
    , verify = \root -> TableViewHelpers.findFeatureTableElmHtml root |> Expect.equal Nothing |> Just
    }


focusIntersectionContent : String -> Verification
focusIntersectionContent expectedContent =
    { description = "the content of the element with the intersection class should be '" ++ expectedContent ++ "'"
    , verify =
        \focusModeWrapper ->
            focusModeWrapper
                |> (focusIntersection.select >> Result.toMaybe)
                |> Maybe.andThen (queryByTagName "textarea" >> ensureSingleton)
                |> Maybe.andThen (getAttributes >> getStringAttribute "value")
                |> Maybe.map (Expect.equal expectedContent)
    }


tableIntersectionContent : String -> String -> String -> Verification
tableIntersectionContent rowDisplayName colDisplayName expectedContent =
    { description = "the table intersection of '" ++ rowDisplayName ++ "' and '" ++ colDisplayName ++ "' is '" ++ expectedContent ++ "'"
    , verify =
        \featureTable ->
            featureTable
                |> ((tableIntersection rowDisplayName colDisplayName).select >> Result.toMaybe)
                |> Maybe.andThen (queryByTagName "textarea" >> ensureSingleton)
                |> Maybe.andThen (getAttributes >> getStringAttribute "value")
                |> Maybe.map (Expect.equal expectedContent)
    }



-- other helpers


clickButton : ElmHtml msg -> Maybe msg
clickButton button =
    HtmlTestExtra.simulate Event.click button
        |> Result.toMaybe


typeInto : String -> ElmHtml msg -> Maybe msg
typeInto content input =
    HtmlTestExtra.simulate (Event.input content) input
        |> Result.toMaybe


featureCardGetFeatureName : ElmHtml msg -> Maybe String
featureCardGetFeatureName featureCard =
    featureCard
        |> (queryByClassName "featureName" >> ensureSingleton)
        |> Maybe.andThen (extractText >> ensureSingleton)



-- elementary selectors


esClassName : String -> ElementarySelectionStep
esClassName className =
    ( "class='" ++ className ++ "'", queryByClassName className >> ensureSingleton )

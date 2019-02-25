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
                    |> select (columnHeader initialSecondFeatureName)
                    |> operate enterFocusMode
                    |> select focusModePanel
                    |> verify (focusedFeatureName initialSecondFeatureName)
        , test "there is a button on the add feature panel" <|
            \() ->
                (initialState initialModel)
                    |> operate (typeNewFeatureName "New Feature")
                    |> operate (typeNewFeatureDescription "New Description")
                    |> operate pressNewFeatureFocusButton
                    |> select focusModePanel
                    |> verify (focusedFeatureName "New Feature")
        ]


focusMode : Test
focusMode =
    describe "In Focus Mode"
        [ test "there is no feature table" <|
            \() ->
                (initialState initialModel)
                    |> focusOn initialSecondFeatureName
                    |> verify noFeatureTableShown
        , test "there are buttons to go to the previous / next feature, the other feature for the intersection is fixed" <|
            \() ->
                (initialState initialModel)
                    |> defaultSelector (Just focusModePanel)
                    |> focusOn initialSecondFeatureName
                    |> snapshot "before"
                    |> operate pressNextFeatureButton
                    |> snapshot "after"
                    |> Expect.all
                        [ verifySnapshot "before" (crossFeatureName initialFirstFeatureName)
                        , verifySnapshot "after" (crossFeatureName initialSecondFeatureName)
                        , verifySnapshot "after" (focusedFeatureName initialSecondFeatureName)
                        ]
        , test "one intersection is shown at a time" <|
            \() ->
                (initialState initialModel)
                    |> select featureTable
                    |> select (tableIntersection initialSecondFeatureName initialFirstFeatureName)
                    |> operate (typeIntoTextarea "First Awesome Content")
                    |> select featureTable
                    |> select (tableIntersection initialSecondFeatureName initialSecondFeatureName)
                    |> operate (typeIntoTextarea "Second Awesome Content")
                    |> defaultSelector (Just focusModePanel)
                    |> focusOn initialSecondFeatureName
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
                    |> focusOn initialSecondFeatureName
                    |> select focusIntersection
                    |> operate (typeIntoTextarea "Awesome content that was edited in focus mode")
                    |> operate pressNextFeatureButton
                    |> operate pressPreviousFeatureButton
                    |> verify (focusIntersectionContent "Awesome content that was edited in focus mode")
        , test "there is a button to return to table view; intersections edited in focus mode are preserved in table view" <|
            \() ->
                (initialState initialModel)
                    |> focusOn initialSecondFeatureName
                    |> select focusModePanel
                    |> select focusIntersection
                    |> operate (typeIntoTextarea "Awesome content edited in focus mode")
                    |> select focusModePanel
                    |> operate pressReturnToTableViewButton
                    |> select featureTable
                    |> verify (tableIntersectionContent initialSecondFeatureName initialFirstFeatureName "Awesome content edited in focus mode")
        , test "export works the same way as in table view (as in, updates immediately if shown while editing)" <|
            \() ->
                (initialState initialModel)
                    |> operate showImportExportPanel
                    |> focusOn initialSecondFeatureName
                    |> select focusIntersection
                    |> operate (typeIntoTextarea "Awesome content that was edited in focus mode")
                    |> verify (importExportContentContains "Awesome content that was edited in focus mode")
        , test "if a new feature is added, the new feature can be reached while cycling through the features" <|
            \() ->
                (initialState initialModel)
                    |> focusOn initialSecondFeatureName
                    |> addNewFeature "New Feature" "New Description"
                    |> defaultSelector (Just focusModePanel)
                    |> select focusModePanel
                    |> Helpers.repeat (operate pressNextFeatureButton) (List.length initialModel.persistent.features)
                    |> verify (crossFeatureName "New Feature")
        , test "hidden features still show up" <|
            \() ->
                (initialState initialModel)
                    |> select featureTable
                    |> select (columnHeader initialSecondFeatureName)
                    |> operate clickHideButton
                    |> focusOn initialFirstFeatureName
                    |> select focusModePanel
                    |> operate pressNextFeatureButton
                    |> select focusModePanel
                    |> verify (crossFeatureName initialSecondFeatureName)
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
    buildSelector "Focus Mode Panel" [ esClassName "focusModeWrapper" ]


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


focusButton : Selector
focusButton =
    buildSelector "Focus Button" [ ( "<button class='focusBtn'>", TableViewHelpers.extractFocusButtonFromHeaderCell ) ]


focusedFeatureCard : Selector
focusedFeatureCard =
    buildSelector "Focused Feature" [ esClassName "focusedFeature" ]


crossFeatureCard : Selector
crossFeatureCard =
    buildSelector "Cross Feature" [ esClassName "crossFeature" ]


featureNameOfCard : Selector
featureNameOfCard =
    buildSelector "the feature's name" [ esClassName "featureName" ]



-- operations


enterFocusMode : Operation
enterFocusMode =
    clickButton focusButton


typeNewFeatureName : String -> Operation
typeNewFeatureName newFeatureName =
    { description = "type '" ++ newFeatureName ++ "' into the new feature name input"
    , select = identitySelector
    , operate = ControlPanelHelpers.updateNewFeatureName newFeatureName
    }


typeNewFeatureDescription : String -> Operation
typeNewFeatureDescription newFeatureDescription =
    { description = "type '" ++ newFeatureDescription ++ "' into the new feature description input"
    , select = identitySelector
    , operate = ControlPanelHelpers.updateNewFeatureDescription newFeatureDescription
    }


pressNewFeatureFocusButton : Operation
pressNewFeatureFocusButton =
    clickButton <| buildSelector "Add Feature and Focus Button" [ esClassName "addFeatureAndFocus" ]


pressNewFeatureButton : Operation
pressNewFeatureButton =
    { description = "Click the Add Feature Button"
    , select = identitySelector
    , operate = ControlPanelHelpers.pressNewFeatureButton
    }


pressNextFeatureButton : Operation
pressNextFeatureButton =
    clickButton <| buildSelector "Next Feature Button" [ esClassName "nextFeature" ]


pressPreviousFeatureButton : Operation
pressPreviousFeatureButton =
    clickButton <| buildSelector "Previous Feature Button" [ esClassName "previousFeature" ]


typeIntoTextarea : String -> Operation
typeIntoTextarea content =
    { description = "type '" ++ content ++ "' into the only textarea"
    , select = textArea
    , operate = typeInto content
    }


pressReturnToTableViewButton : Operation
pressReturnToTableViewButton =
    clickButton <| buildSelector "Return to Table View Button" [ esClassName "returnToTableView" ]


showImportExportPanel : Operation
showImportExportPanel =
    clickButton <|
        buildSelector
            "Show/Hide Model Button"
            [ ( "Show/Hide Model Button", ControlPanelHelpers.findShowHideModelButton ) ]


clickHideButton : Operation
clickHideButton =
    clickButton <|
        buildSelector
            "Hide Feature Button"
            [ ( "Hide Feature Button", TableViewHelpers.extractHideButtonFromHeaderCell ) ]



-- complex selection / operation shortcuts


focusOn : String -> TestState -> TestState
focusOn featureName =
    select featureTable >> select (columnHeader featureName) >> operate enterFocusMode


addNewFeature : String -> String -> TestState -> TestState
addNewFeature newFeatureName newFeatureDescription =
    operate (typeNewFeatureName newFeatureName)
        >> operate (typeNewFeatureDescription newFeatureDescription)
        >> operate pressNewFeatureButton



-- verifications


focusedFeatureName : String -> Verification
focusedFeatureName =
    verifyTextContent (combineSelectors focusedFeatureCard featureNameOfCard)


crossFeatureName : String -> Verification
crossFeatureName =
    verifyTextContent (combineSelectors crossFeatureCard featureNameOfCard)


noFeatureTableShown : Verification
noFeatureTableShown =
    { description = "no feature table is shown"
    , select = identitySelector
    , verify = \root -> TableViewHelpers.findFeatureTableElmHtml root |> Expect.equal Nothing |> Ok
    }


focusIntersectionContent : String -> Verification
focusIntersectionContent =
    verifyStringAttribute (combineSelectors focusIntersection textArea) "value"


tableIntersectionContent : String -> String -> String -> Verification
tableIntersectionContent rowDisplayName colDisplayName =
    verifyStringAttribute
        (combineSelectors (tableIntersection rowDisplayName colDisplayName) textArea)
        "value"


importExportContentContains : String -> Verification
importExportContentContains substring =
    { description = "the exported model contains '" ++ substring ++ "'"
    , select =
        buildSelector
            "Import / Export Model Textbox"
            [ ( "Import / Export Model Textbox", ControlPanelHelpers.findExportImportTextField ) ]
    , verify =
        getAttributes
            >> getStringAttribute "value"
            >> Result.fromMaybe "'value' attribute not found"
            >> Result.map
                (\content ->
                    Expect.true
                        ("expected content to contain '" ++ substring ++ "' but it did not")
                        (String.contains substring content)
                )
    }



-- elementary operations & operation generators


clickButton : Selector -> Operation
clickButton selector =
    { description = "click " ++ selector.selectionName
    , select = selector
    , operate = HtmlTestExtra.simulate Event.click
    }


typeInto : String -> ElmHtml msg -> Result String msg
typeInto content =
    HtmlTestExtra.simulate (Event.input content)



-- elementary selectors


esClassName : String -> ElementarySelectionStep
esClassName className =
    ( "class='" ++ className ++ "'", queryByClassName className >> ensureSingleton )


esTagName : String -> ElementarySelectionStep
esTagName tagName =
    ( "<" ++ tagName ++ "/>", queryByTagName tagName >> ensureSingleton )


textArea : Selector
textArea =
    buildSelector "the only textarea" [ esTagName "textarea" ]



-- elementary verification helpers & verification generators


verifyStringAttribute : Selector -> String -> String -> Verification
verifyStringAttribute selector attributeName expectedValue =
    { description = "the '" ++ attributeName ++ "' attribute of " ++ selector.selectionName ++ " should be '" ++ expectedValue ++ "'"
    , select = selector
    , verify =
        getAttributes
            >> getStringAttribute attributeName
            >> Maybe.map (Expect.equal expectedValue)
            >> Result.fromMaybe ("attribute '" ++ attributeName ++ "' not found")
    }


verifyTextContent : Selector -> String -> Verification
verifyTextContent selector expectedText =
    { description = "the text of " ++ selector.selectionName ++ " should be " ++ expectedText
    , select = selector
    , verify =
        extractText
            >> ensureSingleton
            >> Maybe.map (Expect.equal expectedText)
            >> Result.fromMaybe "unambiguous text not found"
    }

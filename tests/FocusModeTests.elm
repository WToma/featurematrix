module FocusModeTests exposing (..)

import Test exposing (Test, describe, todo, test)
import FeatureMatrixTestFramework exposing (..)
import TableViewHelpers exposing (findFeatureTableElmHtml, findColumnHeaderByName, extractFocusButtonFromHeaderCell)
import ControlPanelHelpers
import HtmlTestExtra exposing (..)
import ElmHtml.Query exposing (queryByClassName, queryByTagName)
import ElmHtml.InternalTypes exposing (ElmHtml)
import Test.Html.Event as Event
import Helpers exposing (ensureSingleton)
import Expect
import TestHelpers exposing (initialModel)


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
        [ todo "there is no feature table"
        , todo "one intersection is shown at a time"
        , todo "that intersection can be edited"
        , todo "there are buttons to go to the previous / next feature, the other feature for the intersection is fixed"
        , todo "there is a button to return to table view"
        , todo "export works the same way"
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
    { selectionName =
        { name = "Feature Table"
        , selectionSteps = [ "<table class='featureTable'>" ]
        }
    , select = TableViewHelpers.findFeatureTableElmHtml
    }


columnHeader : String -> Selector
columnHeader featureDisplayName =
    { selectionName =
        { name = "Column Header called " ++ featureDisplayName
        , selectionSteps = [ "<div>", "where text=" ++ featureDisplayName ]
        }
    , select = TableViewHelpers.findColumnHeaderByName featureDisplayName
    }


focusModePanel : Selector
focusModePanel =
    { selectionName =
        { name = "Focus Mode"
        , selectionSteps = [ "class=focusModeWrapper", "singleton" ]
        }
    , select = queryByClassName "focusModeWrapper" >> ensureSingleton
    }



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



-- verifications


focusedFeature : String -> Verification
focusedFeature expectedFocusedFeatureName =
    { description = "the content of the element with the focusedFeature class should be " ++ expectedFocusedFeatureName
    , verify =
        \focusModeWrapper ->
            focusModeWrapper
                |> (queryByClassName "focusedFeature" >> ensureSingleton)
                |> Maybe.andThen (queryByClassName "featureName" >> ensureSingleton)
                |> Maybe.andThen (extractText >> ensureSingleton)
                |> Maybe.map (Expect.equal expectedFocusedFeatureName)
    }



-- other helpers


clickButton : ElmHtml msg -> Maybe msg
clickButton button =
    HtmlTestExtra.simulate Event.click button
        |> Result.toMaybe

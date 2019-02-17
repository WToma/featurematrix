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
        [ test "there is no feature table" <|
            \() ->
                (initialState initialModel)
                    |> focusOn "Import"
                    |> verify noFeatureTableShown
        , test "there are buttons to go to the previous / next feature, the other feature for the intersection is fixed" <|
            let
                headMust =
                    List.head >> Maybe.map .displayName >> Maybe.withDefault "Bad Test Data"

                firstFeatureName =
                    initialModel.persistent.features |> headMust

                secondFeatureName =
                    initialModel.persistent.features |> List.drop 1 |> headMust
            in
                \() ->
                    (initialState initialModel)
                        |> focusOn "Import"
                        |> select focusModePanel
                        |> snapshot "before"
                        |> operate pressNextFeatureButton
                        |> select focusModePanel
                        |> snapshot "after"
                        |> Expect.all
                            [ verifySnapshot "before" (crossFeature firstFeatureName)
                            , verifySnapshot "after" (crossFeature secondFeatureName)
                            , verifySnapshot "after" (focusedFeature "Import")
                            ]
        , test "one intersection is shown at a time" <|
            \() ->
                (initialState initialModel)
                    |> select featureTable
                    |> select (tableIntersection "Import" "Shows Features")
                    |> operate (typeIntoTextarea "First Awesome Content")
                    |> select featureTable
                    |> select (tableIntersection "Import" "Edit Intersection")
                    |> operate (typeIntoTextarea "Second Awesome Content")
                    |> focusOn "Import"
                    |> select focusModePanel
                    |> snapshot "first focus"
                    |> operate pressNextFeatureButton
                    |> select focusModePanel
                    |> snapshot "second focus"
                    |> Expect.all
                        [ verifySnapshot "first focus" (focusIntersectionContent "First Awesome Content")
                        , verifySnapshot "second focus" (focusIntersectionContent "Second Awesome Content")
                        ]
        , todo "that intersection can be edited"
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


tableIntersection : String -> String -> Selector
tableIntersection rowFeatureDisplayName colFeatureDisplayName =
    { selectionName =
        { name = "The intersection of '" ++ rowFeatureDisplayName ++ "' and '" ++ colFeatureDisplayName ++ "'"
        , selectionSteps =
            [ "establish the row header index of '" ++ rowFeatureDisplayName ++ "' in the feature table"
            , "select that row from the table"
            , "establish the column header index of '" ++ colFeatureDisplayName ++ "' in the row"
            , "select that cell from the row"
            ]
        }
    , select = TableViewHelpers.findIntersectionCell rowFeatureDisplayName colFeatureDisplayName >> Result.toMaybe
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


pressNextFeatureButton : Operation
pressNextFeatureButton =
    { description = "press the button with the nextFeature class"
    , operate = queryByClassName "nextFeature" >> ensureSingleton >> Maybe.andThen clickButton
    }


typeIntoTextarea : String -> Operation
typeIntoTextarea content =
    { description = "type '" ++ content ++ "' into the only textarea of the current selection"
    , operate = queryByTagName "textarea" >> ensureSingleton >> Maybe.andThen (typeInto content)
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
                |> (queryByClassName "intersection" >> ensureSingleton)
                |> Maybe.andThen (extractText >> ensureSingleton)
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

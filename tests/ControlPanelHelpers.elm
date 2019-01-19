module ControlPanelHelpers exposing (findShowHideModelButton, findExportImportTextField, findNewFeatureName, findNewFeatureDescription, findAddNewFeatureButton, findHiddenFeatureByName, findShowFeatureButton)

import ElmHtml.InternalTypes exposing (ElmHtml)
import ElmHtml.Query exposing (queryByTagName, queryByClassName, queryById)
import HtmlTestExtra exposing (extractText)
import Helpers exposing (..)


findShowHideModelButton : ElmHtml msg -> Maybe (ElmHtml msg)
findShowHideModelButton html =
    queryById "hideShowSaveLoadBox" html |> ensureSingleton


findExportImportTextField : ElmHtml msg -> Maybe (ElmHtml msg)
findExportImportTextField html =
    queryByClassName "saveLoadBox" html |> ensureSingleton


findNewFeatureName : ElmHtml msg -> Maybe (ElmHtml msg)
findNewFeatureName html =
    queryByClassName "newFeatureName" html |> ensureSingleton


findNewFeatureDescription : ElmHtml msg -> Maybe (ElmHtml msg)
findNewFeatureDescription html =
    queryByClassName "newFeatureDescription" html |> ensureSingleton


findAddNewFeatureButton : ElmHtml msg -> Maybe (ElmHtml msg)
findAddNewFeatureButton html =
    queryByTagName "button" html
        |> List.filter (\b -> extractText b |> List.member "Add Feature")
        |> ensureSingleton


findHiddenFeatures : ElmHtml msg -> List (ElmHtml msg)
findHiddenFeatures html =
    queryByClassName "hiddenFeatureItem" html


findHiddenFeatureByName : String -> ElmHtml msg -> Maybe (ElmHtml msg)
findHiddenFeatureByName featureName html =
    findHiddenFeatures html
        |> List.filter (\c -> extractText c == [ featureName ])
        |> ensureSingleton


findShowFeatureButton : ElmHtml msg -> Maybe (ElmHtml msg)
findShowFeatureButton hiddenFeature =
    queryByTagName "button" hiddenFeature |> ensureSingleton

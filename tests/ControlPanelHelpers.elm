module ControlPanelHelpers exposing (..)

import ElmHtml.InternalTypes exposing (ElmHtml)
import ElmHtml.Query exposing (queryByTagName, queryByClassName, queryById)
import HtmlTestExtra exposing (extractText, getStringAttribute)
import Helpers exposing (..)
import Main
import Model
import Msg
import Test.Html.Event as Event


showImportExportPanel : Model.Model -> Result String Model.Model
showImportExportPanel initialModel =
    render initialModel
        |> findShowHideModelButton
        |> Result.fromMaybe "show/hide model button not found"
        |> Result.andThen (HtmlTestExtra.simulate Event.click)
        |> Result.map ((flip Main.update) initialModel)


getImportExportContent : ElmHtml msg -> Result String String
getImportExportContent html =
    findExportImportTextField html
        |> Result.fromMaybe "import/export textarea not found"
        |> Result.map HtmlTestExtra.getAttributes
        |> Result.map (getStringAttribute "value")
        |> Result.map (Maybe.withDefault "")


updateImportExportContent : String -> ElmHtml Msg.Msg -> Result String Msg.Msg
updateImportExportContent newContent html =
    findExportImportTextField html
        |> Result.fromMaybe "import/export textarea not found"
        |> Result.andThen (HtmlTestExtra.simulate (Event.input newContent))


importAndUpdateModel : String -> Model.Model -> Result String Model.Model
importAndUpdateModel typedText initialModel =
    showImportExportPanel initialModel
        |> Result.map render
        |> Result.andThen (updateImportExportContent typedText)
        |> Result.map ((flip Main.update) initialModel)


addFeatureAndUpdateModel : String -> String -> Model.Model -> Result String Model.Model
addFeatureAndUpdateModel newFeatureName newFeatureDescription initialModel =
    let
        chainedUpdateReducer : (ElmHtml Msg.Msg -> Result String Msg.Msg) -> Result String Model.Model -> Result String Model.Model
        chainedUpdateReducer msgGenerator model =
            let
                rendered =
                    Result.map render model

                msg =
                    Result.andThen msgGenerator rendered
            in
                Result.map2 Main.update msg model
    in
        List.foldl chainedUpdateReducer
            (Result.Ok initialModel)
            [ updateNewFeatureName newFeatureName
            , updateNewFeatureDescription newFeatureDescription
            , pressNewFeatureButton
            ]


updateNewFeatureName : String -> ElmHtml Msg.Msg -> Result String Msg.Msg
updateNewFeatureName newFeatureName html =
    findNewFeatureName html
        |> Result.fromMaybe "new feature name textfield not found"
        |> Result.andThen (HtmlTestExtra.simulate (Event.input newFeatureName))


updateNewFeatureDescription : String -> ElmHtml Msg.Msg -> Result String Msg.Msg
updateNewFeatureDescription newFeatureDescription html =
    findNewFeatureDescription html
        |> Result.fromMaybe "new feature description textfield not found"
        |> Result.andThen (HtmlTestExtra.simulate (Event.input newFeatureDescription))


pressNewFeatureButton : ElmHtml Msg.Msg -> Result String Msg.Msg
pressNewFeatureButton html =
    findAddNewFeatureButton html
        |> Result.fromMaybe "new feature button not found"
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


showFeature : String -> Model.Model -> Result String Model.Model
showFeature featureName model =
    model
        |> render
        |> pressShowFeatureButton featureName
        |> Result.map ((flip Main.update) model)


pressShowFeatureButton : String -> ElmHtml Msg.Msg -> Result String Msg.Msg
pressShowFeatureButton featureName html =
    findHiddenFeatureByName featureName html
        |> Result.fromMaybe ("hidden feature " ++ featureName ++ " not found")
        |> Result.map findShowFeatureButton
        |> Result.andThen (Result.fromMaybe ("show feature button not found for hidden feature " ++ featureName))
        |> Result.andThen (HtmlTestExtra.simulate Event.click)


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


render : Model.Model -> ElmHtml Msg.Msg
render model =
    Main.view model |> HtmlTestExtra.fromHtml

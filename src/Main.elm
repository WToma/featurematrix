module Main exposing (Model, update, view, dummyFeatures, allFeaturesVisible, Display(..))

import Html exposing (Html, button, div, text, textarea, h5, ul, li, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, placeholder)
import Dict exposing (Dict)
import PersistentModel exposing (PersistentModel, Feature, encodePersistentModel, decodePersistentModel, isFeatureVisible, addNewFeature)
import Msg exposing (Msg(..))
import TableView exposing (renderFeatureTable)
import NewFeaturePanel
import FocusMode
import Helpers exposing (flattenMaybeList)


type Display
    = Table
    | Focus FocusMode.Model
    | DisplayError String


type alias Model =
    { persistent : PersistentModel
    , parseError : Maybe String
    , showSerialized : Bool
    , newFeaturePanelState : NewFeaturePanel.Model
    , display : Display
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model =
            { persistent =
                { features = dummyFeatures
                , featureVisibility = allFeaturesVisible dummyFeatures
                , intersections = Dict.empty
                }
            , parseError = Nothing
            , showSerialized = False
            , newFeaturePanelState = NewFeaturePanel.initialModel
            , display = Table
            }
        , view = view
        , update = update
        }


dummyFeatures : List Feature
dummyFeatures =
    [ { featureId = "showFeatures"
      , displayName = "Shows Features"
      , description = "Shows features as a Table"
      }
    , { featureId = "edit"
      , displayName = "Edit Intersection"
      , description = "Type Features into the Cells of the Table"
      }
    , { featureId = "export"
      , displayName = "Export"
      , description = "Export the Current Model into JSON"
      }
    , { featureId = "import"
      , displayName = "Import"
      , description = "Import the Model from JSON"
      }
    , { featureId = "addFeature"
      , displayName = "Add Feature"
      , description = "A New Feature can be Added With Description"
      }
    , { featureId = "hideFeature"
      , displayName = "Hide Feature"
      , description = "Remove a Feature from the Feature Table, and Provide Button to Show Again"
      }
    ]


allFeaturesVisible : List Feature -> Dict String Bool
allFeaturesVisible features =
    List.map (\f -> ( f.featureId, True )) features |> Dict.fromList


update : Msg -> Model -> Model
update msg model =
    case msg of
        IntersectionUpdated smallerId largerId newValue ->
            if newValue /= "" then
                let
                    oldPersistent =
                        model.persistent

                    newPersistent =
                        { oldPersistent | intersections = Dict.insert ( smallerId, largerId ) newValue oldPersistent.intersections }
                in
                    { model | persistent = newPersistent }
            else
                let
                    oldPersistent =
                        model.persistent

                    newPersistent =
                        { oldPersistent | intersections = Dict.remove ( smallerId, largerId ) model.persistent.intersections }
                in
                    { model | persistent = newPersistent }

        HideModel ->
            { model | showSerialized = False }

        ShowModel ->
            { model | showSerialized = True }

        SerializedModelUpdated str ->
            case decodePersistentModel str of
                Ok newPersistent ->
                    { model | persistent = newPersistent, parseError = Nothing }

                Err failures ->
                    { model | parseError = Just (String.join "; " failures) }

        AddNewFeature request ->
            case addNewFeature model.persistent request.shortName request.description of
                Result.Ok ( newPersistentModel, newFeature ) ->
                    let
                        addedFeatureModel =
                            { model
                                | persistent = newPersistentModel
                                , newFeaturePanelState = NewFeaturePanel.initialModel
                            }

                        maybeFocusMsg =
                            if request.enterFocusMode then
                                Just (FocusFeature newFeature.featureId)
                            else
                                Nothing
                    in
                        maybeFocusMsg
                            |> Maybe.map ((flip update) addedFeatureModel)
                            |> Maybe.withDefault addedFeatureModel

                Result.Err reason ->
                    { model | newFeaturePanelState = NewFeaturePanel.setError reason model.newFeaturePanelState }

        HideFeature featureId ->
            let
                newFeatureVisibility =
                    Dict.insert featureId False model.persistent.featureVisibility

                oldPersistent =
                    model.persistent

                newPersistent =
                    { oldPersistent | featureVisibility = newFeatureVisibility }
            in
                { model | persistent = newPersistent }

        ShowFeature featureId ->
            let
                newFeatureVisibility =
                    Dict.insert featureId True model.persistent.featureVisibility

                oldPersistent =
                    model.persistent

                newPersistent =
                    { oldPersistent | featureVisibility = newFeatureVisibility }
            in
                { model | persistent = newPersistent }

        NFPMsg nfpMsg ->
            let
                ( nfpState, maybeMessage ) =
                    NewFeaturePanel.update AddNewFeature nfpMsg model.newFeaturePanelState

                newModel =
                    { model | newFeaturePanelState = nfpState }
            in
                Maybe.map (\msg -> update msg newModel) maybeMessage |> Maybe.withDefault newModel

        FocusFeature focusFeatureId ->
            let
                focusModel =
                    FocusMode.init model.persistent focusFeatureId

                newDisplay =
                    focusModel
                        |> Maybe.map Focus
                        -- TODO give useful error message or get rid of this condition
                        |> Maybe.withDefault (DisplayError "focus mode cannot be displayed")
            in
                { model | display = newDisplay }

        FocusModeMsg focusMsg ->
            case model.display of
                Focus focusModel ->
                    { model | display = Focus (FocusMode.update focusMsg focusModel) }

                _ ->
                    model


view : Model -> Html Msg
view model =
    appContainer
        (renderControlPanel model)
        (case model.display of
            DisplayError error ->
                div [ class "alert alert-danger" ] [ text error ]

            Table ->
                renderFeatureTable model.persistent model.parseError

            Focus focusModel ->
                FocusMode.view focusModel |> Html.map FocusModeMsg
        )


appContainer : Html Msg -> Html Msg -> Html Msg
appContainer controlPanel mainPanel =
    div [ class "row h-100" ]
        [ div [ class "controlPanel col" ] [ controlPanel ]
        , div [ class "featurePanel col-8" ] [ mainPanel ]
        ]


renderControlPanel : Model -> Html Msg
renderControlPanel model =
    div [ class "controlPanelWrapper h-100 border-right" ] <|
        flattenMaybeList
            [ Just <| renderModelInputOutput model
            , Just (NewFeaturePanel.view model.newFeaturePanelState |> Html.map NFPMsg)
            , renderHiddenFeatures model.persistent
            ]


renderModelInputOutput : Model -> Html Msg
renderModelInputOutput model =
    let
        maybeTextArea =
            if model.showSerialized then
                Just
                    (div [ class "input-group mb-3" ]
                        [ textarea [ class "saveLoadBox form-control", value (encodePersistentModel model.persistent), readonly False, onInput SerializedModelUpdated ] [] ]
                    )
            else
                Nothing

        showHideButton =
            Just
                (div [ class "mb-3" ]
                    [ button
                        [ onClick
                            (if model.showSerialized then
                                HideModel
                             else
                                ShowModel
                            )
                        , id "hideShowSaveLoadBox"
                        , class "btn btn-primary"
                        ]
                        [ text
                            (if model.showSerialized then
                                "Hide"
                             else
                                "Show"
                            )
                        ]
                    ]
                )

        cardBody =
            flattenMaybeList [ maybeTextArea, showHideButton ]
    in
        div [ class "card" ]
            [ div [ class "card-body" ] cardBody
            ]


renderHiddenFeatures : PersistentModel -> Maybe (Html Msg)
renderHiddenFeatures model =
    let
        hiddenFeatures =
            List.filter (\f -> not (isFeatureVisible model f)) model.features

        hiddenFeaturesExist =
            not (List.isEmpty hiddenFeatures)

        renderShowFeature =
            \f ->
                li [ class "hiddenFeatureItem list-group-item" ]
                    [ div [ class "input-group" ]
                        [ div [ class "input-group-prepend" ] [ span [ class "featureName input-group-text" ] [ text f.displayName ] ]
                        , button [ class "btn btn-primary btn-sm", onClick (ShowFeature f.featureId) ] [ text "Show" ]
                        ]
                    ]
    in
        if hiddenFeaturesExist then
            Just
                (div [ class "card" ]
                    [ div [ class "card-body" ]
                        [ h5 [ class "card-title" ] [ text "Features Hidden from Display" ]
                        , ul [ class "list-group list-group-flush" ] (List.map renderShowFeature hiddenFeatures)
                        ]
                    ]
                )
        else
            Nothing

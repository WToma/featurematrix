module Main exposing (Model, update, view, dummyFeatures, allFeaturesVisible, Display(..))

import Html exposing (Html, button, div, text, textarea, h2)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, placeholder)
import Dict exposing (Dict)
import PersistentModel exposing (PersistentModel, Feature, encodePersistentModel, decodePersistentModel, isFeatureVisible, addNewFeature)
import Msg exposing (Msg(..))
import TableView exposing (renderFeatureTable)
import NewFeaturePanel


type Display = Table | DisplayError String

type alias Model =
    { persistent : PersistentModel
    , parseError : Maybe String
    , showSerialized : Bool
    , newFeaturePanelState : NewFeaturePanel.Model
    , display: Display
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
                Result.Ok newPersistentModel ->
                    { model
                        | persistent = newPersistentModel
                        , newFeaturePanelState = NewFeaturePanel.initialModel
                    }

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


view : Model -> Html Msg
view model =
    appContainer
        (renderModelInputOutput model)
        ( case model.display of
            DisplayError error ->
                div [] [ text error ]
            Table -> renderFeatureTable model.persistent model.parseError
        )


appContainer : Html Msg -> Html Msg -> Html Msg
appContainer controlPanel mainPanel =
    div [ class "appContainer " ]
        [ div [ class "controlPanel" ] [ controlPanel ]
        , div [ class "featurePanel" ] [ mainPanel ]
        ]


renderModelInputOutput : Model -> Html Msg
renderModelInputOutput model =
    div [ class "controlPanelWrapper" ]
        [ if model.showSerialized then
            textarea [ class "saveLoadBox", value (encodePersistentModel model.persistent), readonly False, onInput SerializedModelUpdated ] []
          else
            text ""
        , button
            [ onClick
                (if model.showSerialized then
                    HideModel
                 else
                    ShowModel
                )
            , id "hideShowSaveLoadBox"
            ]
            [ text
                (if model.showSerialized then
                    "Hide"
                 else
                    "Show"
                )
            ]
        , NewFeaturePanel.view NFPMsg model.newFeaturePanelState
        , renderHiddenFeatures model.persistent
        ]


renderHiddenFeatures : PersistentModel -> Html Msg
renderHiddenFeatures model =
    let
        hiddenFeatures =
            List.filter (\f -> not (isFeatureVisible model f)) model.features

        hiddenFeaturesExist =
            not (List.isEmpty hiddenFeatures)

        renderShowFeature =
            \f -> div [ class "hiddenFeatureItem" ] [ text f.displayName, button [ onClick (ShowFeature f.featureId) ] [ text "(show)" ] ]
    in
        div []
            (if hiddenFeaturesExist then
                [ h2 [] [ text "Features Hidden from Display" ] ] ++ (List.map renderShowFeature hiddenFeatures)
             else
                []
            )

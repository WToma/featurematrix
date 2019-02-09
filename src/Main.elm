module Main exposing (update, view, dummyFeatures, allFeaturesVisible)

import Html exposing (Html, button, div, text, textarea, h2)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, placeholder)
import Dict exposing (Dict)
import Model exposing (Model, PersistentModel, Feature, encodePersistentModel, decodePersistentModel, isFeatureVisible)
import Msg exposing (Msg(..))
import TableView exposing (renderFeatureTable)
import Helpers exposing (phraseToCamelCase)


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
            , newFeaturePanelState =
                { shortName = ""
                , description = ""
                , errorAdding = Nothing
                }
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

        NFPShortNameUpdated str ->
            let
                currentNfpState =
                    model.newFeaturePanelState

                updatedNfpState =
                    { currentNfpState | shortName = str, errorAdding = Nothing }
            in
                { model | newFeaturePanelState = updatedNfpState }

        NFPDescriptionUpdated str ->
            let
                currentNfpState =
                    model.newFeaturePanelState

                updatedNfpState =
                    { currentNfpState | description = str, errorAdding = Nothing }
            in
                { model | newFeaturePanelState = updatedNfpState }

        AddNewFeature shortName description ->
            case addNewFeature model.persistent.features model.persistent.featureVisibility shortName description of
                Result.Ok ( newFeatures, newVisibility ) ->
                    let
                        oldPersistent =
                            model.persistent

                        newPersistent =
                            { oldPersistent | features = newFeatures, featureVisibility = newVisibility }
                    in
                        { model | persistent = newPersistent }

                Result.Err reason ->
                    let
                        currentNfpState =
                            model.newFeaturePanelState

                        updatedNfpState =
                            { currentNfpState | errorAdding = Just reason }
                    in
                        { model | newFeaturePanelState = updatedNfpState }

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


getUniqueFeatureId : List String -> String -> Int -> String
getUniqueFeatureId takens base try =
    let
        suffix =
            if try == 0 then
                ""
            else
                toString try

        candidate =
            base ++ suffix

        taken =
            List.any (\x -> x == candidate) takens
    in
        if taken then
            getUniqueFeatureId takens base (try + 1)
        else
            candidate


addNewFeature : List Feature -> Dict String Bool -> String -> String -> Result String ( List Feature, Dict String Bool )
addNewFeature currentFeatures featureVisibility newShortName newDescription =
    let
        isShortNameEmpty =
            String.isEmpty newShortName

        isDescriptionEmpty =
            String.isEmpty newDescription

        isShortNameTaken =
            List.any (\f -> f.displayName == newShortName) currentFeatures
    in
        if (not isShortNameTaken) && (not isDescriptionEmpty) && (not isShortNameEmpty) then
            Result.Ok
                (let
                    featureId =
                        getUniqueFeatureId (List.map (\f -> f.featureId) currentFeatures) (phraseToCamelCase newShortName) 0

                    newFeature =
                        { featureId = featureId, displayName = newShortName, description = newDescription }
                 in
                    ( List.append currentFeatures [ newFeature ], Dict.insert featureId True featureVisibility )
                )
        else
            Result.Err
                (if isShortNameTaken then
                    "a feature with this name already exists"
                 else if isShortNameEmpty then
                    "the feature name cannot be empty"
                 else
                    -- isDescriptonEmpty
                    "the description cannot be empty"
                )


view : Model -> Html Msg
view model =
    appContainer
        (renderModelInputOutput model)
        (renderFeatureTable model)


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
        , renderFeatureAdd model
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


renderFeatureAdd : Model -> Html Msg
renderFeatureAdd model =
    div [ class "featureAdd" ]
        [ h2 [] [ text "Add New Feature" ]
        , Html.input [ type_ "text", placeholder "Short Name of the New Feature", class "newFeatureName", value model.newFeaturePanelState.shortName, onInput NFPShortNameUpdated ] []
        , textarea [ class "newFeatureDescription", placeholder "A description of the new feature.", value model.newFeaturePanelState.description, onInput NFPDescriptionUpdated ] []
        , button [ onClick (AddNewFeature model.newFeaturePanelState.shortName model.newFeaturePanelState.description) ] [ text "Add Feature" ]
        , text (Maybe.withDefault "" (Maybe.map (\reason -> "Error Adding New Feature: " ++ reason) model.newFeaturePanelState.errorAdding))
        ]

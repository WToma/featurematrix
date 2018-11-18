module Main exposing (..)

import Html exposing (Html, button, div, text, textarea, h2)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, placeholder)
import Dict exposing (Dict)
import Char
import Json.Encode as JE
import Json.Decode as JD


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model =
            { features = dummyFeatures
            , featureVisibility = allFeaturesVisible dummyFeatures
            , intersections = Dict.empty
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


type alias Model =
    { features : List Feature
    , featureVisibility : Dict String Bool
    , intersections : Dict ( String, String ) String
    , parseError : Maybe String
    , showSerialized : Bool
    , newFeaturePanelState :
        { shortName : String
        , description : String
        , errorAdding : Maybe String
        }
    }


type Msg
    = IntersectionUpdated String String String
    | ShowModel
    | HideModel
    | SerializedModelUpdated String
    | NFPShortNameUpdated String
    | NFPDescriptionUpdated String
    | AddNewFeature String String
    | HideFeature String
    | ShowFeature String


type alias Feature =
    { featureId : String
    , displayName : String
    , description : String
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


isFeatureVisible : Dict String Bool -> Feature -> Bool
isFeatureVisible featureVisibilities feature =
    Dict.get feature.featureId featureVisibilities |> Maybe.withDefault False


renderFeatureTableGeneric : (( Feature, Feature ) -> Html Msg) -> List Feature -> Dict String Bool -> Html Msg
renderFeatureTableGeneric intersectionRenderer allFeatures featureVisibilities =
    let
        featuresToRender =
            List.filter (isFeatureVisible featureVisibilities) allFeatures

        hasFeaturesToRender =
            not (List.isEmpty featuresToRender)
    in
        div []
            (if hasFeaturesToRender then
                [ Html.table [ class "featureTable" ]
                    (-- header
                     [ Html.tr [] ([ Html.th [ class "featureTable" ] [] ] ++ List.map (\f -> Html.th [ class "featureTable" ] [ renderFeatureHeader f ]) featuresToRender) ]
                        ++ -- rows
                           List.map
                            (\f ->
                                Html.tr [] ([ Html.th [ class "featureTable" ] [ renderFeatureHeader f ] ] ++ List.map (\f2 -> Html.td [ class "featureTable" ] [ intersectionRenderer ( f, f2 ) ]) featuresToRender)
                            )
                            featuresToRender
                    )
                ]
             else
                [ text "No features displayed. Add a new feature or show an existing one using the left panel" ]
            )


renderFeatureHeader : Feature -> Html Msg
renderFeatureHeader f =
    div [] [ text f.displayName, button [ onClick (HideFeature f.featureId) ] [ text "(hide)" ] ]


renderIntersectionEditBox : Dict ( String, String ) String -> ( Feature, Feature ) -> Html Msg
renderIntersectionEditBox intersectionValues ( f1, f2 ) =
    let
        smallerId =
            if f1.featureId < f2.featureId then
                f1.featureId
            else
                f2.featureId

        largerId =
            if f1.featureId < f2.featureId then
                f2.featureId
            else
                f1.featureId

        inputId =
            smallerId ++ "_vs_" ++ largerId

        intersectionValue =
            Maybe.withDefault "" (Dict.get ( smallerId, largerId ) intersectionValues)
    in
        Html.textarea [ class "intersectionTextArea", id inputId, value intersectionValue, onInput (IntersectionUpdated smallerId largerId) ] []


update : Msg -> Model -> Model
update msg model =
    case msg of
        IntersectionUpdated smallerId largerId newValue ->
            if newValue /= "" then
                { model | intersections = Dict.insert ( smallerId, largerId ) newValue model.intersections }
            else
                { model | intersections = Dict.remove ( smallerId, largerId ) model.intersections }

        HideModel ->
            { model | showSerialized = False }

        ShowModel ->
            { model | showSerialized = True }

        SerializedModelUpdated str ->
            case decodeModel str of
                Ok newIntersections ->
                    { model | intersections = newIntersections, parseError = Nothing }

                Err jsonParsingFailure ->
                    { model | parseError = Just jsonParsingFailure }

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
            case addNewFeature model.features model.featureVisibility shortName description of
                Result.Ok ( newFeatures, newVisibility ) ->
                    { model | features = newFeatures, featureVisibility = newVisibility }

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
                    Dict.insert featureId False model.featureVisibility
            in
                { model | featureVisibility = newFeatureVisibility }

        ShowFeature featureId ->
            let
                newFeatureVisibility =
                    Dict.insert featureId True model.featureVisibility
            in
                { model | featureVisibility = newFeatureVisibility }


mapFirst : (a -> a) -> List a -> List a
mapFirst f xs =
    case xs of
        [] ->
            []

        head :: tail ->
            (f head) :: tail


phraseToCamelCase : String -> String
phraseToCamelCase phrase =
    let
        words =
            String.words phrase

        firstCharToLower =
            \s -> String.fromList (mapFirst Char.toLocaleLower (String.toList s))

        firstCharToUpper =
            \s -> String.fromList (mapFirst Char.toLocaleUpper (String.toList s))

        allFirstUpper =
            List.map firstCharToUpper words

        camelCaseWords =
            mapFirst firstCharToLower allFirstUpper
    in
        List.foldr (++) "" camelCaseWords


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
        (renderMainArea model)


renderMainArea : Model -> Html Msg
renderMainArea model =
    div [ class "featureTableContainer" ]
        [ div [] [ text (Maybe.withDefault "" model.parseError) ]
        , renderFeatureTableGeneric (renderIntersectionEditBox model.intersections) model.features model.featureVisibility
        ]


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
            textarea [ class "saveLoadBox", value (encodeModel model.intersections), readonly False, onInput SerializedModelUpdated ] []
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
        , renderHiddenFeatures model
        ]


renderHiddenFeatures : Model -> Html Msg
renderHiddenFeatures model =
    let
        hiddenFeatures =
            List.filter (\f -> not (isFeatureVisible model.featureVisibility f)) model.features

        hiddenFeaturesExist =
            not (List.isEmpty hiddenFeatures)

        renderShowFeature =
            \f -> div [] [ text f.displayName, button [ onClick (ShowFeature f.featureId) ] [ text "(show)" ] ]
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


encodeModel : Dict ( String, String ) String -> String
encodeModel intersectionValues =
    JE.encode 4 (JE.list (List.map encodeIntersectionEntry (Dict.toList intersectionValues)))


encodeIntersectionEntry : ( ( String, String ), String ) -> JE.Value
encodeIntersectionEntry ( ( smallerKey, largerKey ), value ) =
    JE.object
        [ ( "smallerKey", JE.string smallerKey )
        , ( "largerKey", JE.string largerKey )
        , ( "value", JE.string value )
        ]


decodeModel : String -> Result String (Dict ( String, String ) String)
decodeModel =
    JD.decodeString parseModel


parseModel : JD.Decoder (Dict ( String, String ) String)
parseModel =
    JD.map Dict.fromList (JD.list parseIntersectionEntry)


parseIntersectionEntry : JD.Decoder ( ( String, String ), String )
parseIntersectionEntry =
    let
        mkEntry smallerKey largerKey value =
            ( ( smallerKey, largerKey ), value )

        smallerKeyDec =
            JD.field "smallerKey" JD.string

        largerKeyDec =
            JD.field "largerKey" JD.string

        valueDec =
            JD.field "value" JD.string
    in
        JD.map3 mkEntry smallerKeyDec largerKeyDec valueDec

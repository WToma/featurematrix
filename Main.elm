module Main exposing (..)

import Html exposing (Html, button, div, text, textarea)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly)
import Dict exposing (Dict)
import Json.Encode as JE
import Json.Decode as JD


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model =
            { features = dummyFeatures
            , intersections = Dict.empty
            , parseError = Nothing
            , showSerialized = False
            }
        , view = view
        , update = update
        }


type alias Model =
    { features : List Feature
    , intersections : Dict ( String, String ) String
    , parseError : Maybe String
    , showSerialized : Bool
    }


type Msg
    = IntersectionUpdated String String String
    | ShowModel
    | HideModel
    | SerializedModelUpdated String


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
    ]


renderFeatureTableGeneric : (( Feature, Feature ) -> Html msg) -> List Feature -> Html msg
renderFeatureTableGeneric intersectionRenderer features =
    Html.table [ class "featureTable" ]
        (-- header
         [ Html.tr [] ([ Html.th [ class "featureTable" ] [] ] ++ List.map (\f -> Html.th [ class "featureTable" ] [ Html.text f.displayName ]) features) ]
            ++ -- rows
               List.map
                (\f ->
                    Html.tr [] ([ Html.th [ class "featureTable" ] [ Html.text f.displayName ] ] ++ List.map (\f2 -> Html.td [ class "featureTable" ] [ intersectionRenderer ( f, f2 ) ]) features)
                )
                features
        )


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
        Html.input [ type_ "text", id inputId, value intersectionValue, onInput (IntersectionUpdated smallerId largerId) ] []


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


view : Model -> Html Msg
view model =
    appContainer
        (renderModelInputOutput model)
        (renderMainArea model)


renderMainArea : Model -> Html Msg
renderMainArea model =
    div [ class "featureTableContainer" ]
        [ div [] [ text (Maybe.withDefault "" model.parseError) ]
        , renderFeatureTableGeneric (renderIntersectionEditBox model.intersections) model.features
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
            ]
            [ text
                (if model.showSerialized then
                    "Hide"
                 else
                    "Show"
                )
            ]
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

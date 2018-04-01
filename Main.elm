module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


main : Program Never number Msg
main =
    Html.beginnerProgram { model = 0, view = view, update = update }


type Msg
    = Increment
    | Decrement


type alias Feature =
    { featureId : String
    , displayName : String
    , description : String
    }


dummyFeatures : List Feature
dummyFeatures =
    [ { featureId = "showFeatures"
      , displayName = "Shows Features as a Table"
      , description = "Shows features as a Table"
      }
    , { featureId = "focus"
      , displayName = "Focus on Selected Features"
      , description = "Select which features to display"
      }
    ]


renderFeatureTable : List Feature -> Html msg
renderFeatureTable features =
    Html.table [ class "featureTable" ]
        (-- header
         [ Html.tr [] ([ Html.th [ class "featureTable" ] [] ] ++ List.map (\f -> Html.th [ class "featureTable" ] [ Html.text f.displayName ]) features) ]
            ++ -- rows
               List.map
                (\f ->
                    Html.tr [] ([ Html.th [ class "featureTable" ] [ Html.text f.displayName ] ] ++ List.map (\f2 -> Html.td [ class "featureTable" ] []) features)
                )
                features
        )


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : a -> Html Msg
view model =
    div []
        -- [ button [ onClick Decrement ] [ text "-" ]
        -- , div [] [ text (toString model) ]
        -- , button [ onClick Increment ] [ text "+" ]
        -- ]
        [ renderFeatureTable dummyFeatures ]

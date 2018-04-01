module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


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


renderFeatureTable : List Feature -> Html msg
renderFeatureTable features =
    Html.table [ class "featureTable" ]
        (-- header
         [ Html.tr [] ([ Html.th [] [] ] ++ List.map (\f -> Html.th [] [ Html.text f.featureId ]) features) ]
            ++ -- rows
               List.map
                (\f ->
                    Html.tr [] ([ Html.th [] [ Html.text f.featureId ] ] ++ List.map (\f2 -> Html.th [] []) features)
                )
                features
        )


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]

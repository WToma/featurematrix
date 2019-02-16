module FocusMode exposing (Model, init, view)

import PersistentModel exposing (Feature, PersistentModel)
import Html exposing (Html, div, text, table, h5, tr, th, td, span)
import Html.Attributes exposing (class)
import Helpers exposing (ensureSingleton)


-- exported types


type alias Model =
    { persistentModel : PersistentModel
    , focusedFeature : Feature
    , crossFeature : Feature
    }



-- exported functions


init : PersistentModel -> String -> Maybe Model
init persistentModel focusedFeatureId =
    let
        focusedFeature =
            List.filter (\f -> f.featureId == focusedFeatureId) persistentModel.features |> ensureSingleton

        crossFeature =
            List.head persistentModel.features
    in
        Maybe.map2 (\f c -> { persistentModel = persistentModel, focusedFeature = f, crossFeature = c }) focusedFeature crossFeature


view : Model -> Html msg
view model =
    div [ class "focusModeWrapper" ]
        [ table [ class "focusTable table" ]
            [ tr []
                [ th [ class "focusTable" ] [] -- empty
                , th [ class "focusTable" ]
                    [ -- focused feature
                      renderFeatureCard model.focusedFeature "focusedFeature"
                    ]
                ]
            , tr []
                [ th [ class "focusTable" ]
                    [ -- cross feature
                      renderFeatureCard model.crossFeature "crossFeature"
                    ]
                , td [ class "focusTable" ]
                    [ -- intersection
                      div [ class "intersection" ] [ text (displayedIntersection model) ]
                    ]
                ]
            ]
        ]



-- unexported functions


renderFeatureCard : Feature -> String -> Html msg
renderFeatureCard feature markerClass =
    div [ class (markerClass ++ " card cardNoBorder") ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title featureName" ] [ text feature.displayName ]
            , span [ class "card-subtitle text-muted" ] [ text feature.description ]
            ]
        ]


displayedIntersection : Model -> String
displayedIntersection model =
    PersistentModel.getIntersection model.focusedFeature model.crossFeature model.persistentModel
        |> Maybe.withDefault ""

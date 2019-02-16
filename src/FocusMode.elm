module FocusMode exposing (Model, init, view)

import PersistentModel exposing (Feature, PersistentModel)
import Html exposing (Html, div, text, h2)
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
        [ div [ class "focusedFeature" ] [ h2 [] [ text model.focusedFeature.displayName ] ]
        , div [ class "crossFeature" ] [ h2 [] [ text model.crossFeature.displayName ] ]
        , div [ class "intersection" ] [ text (displayedIntersection model) ]
        ]



-- unexported functions


displayedIntersection : Model -> String
displayedIntersection model =
    PersistentModel.getIntersection model.focusedFeature model.crossFeature model.persistentModel
        |> Maybe.withDefault ""

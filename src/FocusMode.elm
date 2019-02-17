module FocusMode exposing (Model, Msg, init, view, update)

import PersistentModel exposing (Feature, PersistentModel)
import Html exposing (Html, div, text, table, h5, tr, th, td, span, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Helpers exposing (ensureSingleton, flattenMaybeList, window1)


-- exported types


type alias Model =
    { persistentModel : PersistentModel
    , focusedFeature : Feature
    , crossFeature : Feature
    }


type Msg
    = PreviousCrossFeature
    | NextCrossFeature



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


view : Model -> Html Msg
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
                      renderCrossFeature model
                    ]
                , td [ class "focusTable" ]
                    [ -- intersection
                      div [ class "intersection" ] [ text (displayedIntersection model) ]
                    ]
                ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        PreviousCrossFeature ->
            let
                ( maybePrevFeature, _ ) =
                    prevNextCrossFeatures model
            in
                maybePrevFeature
                    |> Maybe.map (\f -> { model | crossFeature = f })
                    |> Maybe.withDefault model

        NextCrossFeature ->
            let
                ( _, maybeNextFeature ) =
                    prevNextCrossFeatures model
            in
                maybeNextFeature
                    |> Maybe.map (\f -> { model | crossFeature = f })
                    |> Maybe.withDefault model



-- unexported functions


renderFeatureCard : Feature -> String -> Html msg
renderFeatureCard feature markerClass =
    div [ class (markerClass ++ " card cardNoBorder") ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title featureName" ] [ text feature.displayName ]
            , span [ class "card-subtitle text-muted" ] [ text feature.description ]
            ]
        ]


renderCrossFeature : Model -> Html Msg
renderCrossFeature model =
    let
        ( maybePreviousFeature, maybeNextFeature ) =
            prevNextCrossFeatures model

        previousExists =
            maybePreviousFeature /= Nothing

        nextExists =
            maybeNextFeature /= Nothing

        maybePreviousButton =
            if previousExists then
                Just (div [ class "col col-1 mr-2" ] [ button [ class "previousFeature btn btn-sm btn-primary", onClick PreviousCrossFeature ] [ text "<" ] ])
            else
                Nothing

        maybeNextButton =
            if nextExists then
                Just (div [ class "col col-1 ml-2" ] [ button [ class "nextFeature btn btn-sm btn-primary", onClick NextCrossFeature ] [ text ">" ] ])
            else
                Nothing

        contents =
            [ maybePreviousButton
            , Just (renderFeatureCard model.crossFeature "crossFeature")
            , maybeNextButton
            ]
    in
        div [ class "row align-items-center" ] (flattenMaybeList contents)


displayedIntersection : Model -> String
displayedIntersection model =
    PersistentModel.getIntersection model.focusedFeature model.crossFeature model.persistentModel
        |> Maybe.withDefault ""


prevNextCrossFeatures : Model -> ( Maybe Feature, Maybe Feature )
prevNextCrossFeatures model =
    let
        ( maybePreviousFeature, _, maybeNextFeature ) =
            window1 (\f -> f.featureId == model.crossFeature.featureId) model.persistentModel.features
    in
        ( maybePreviousFeature, maybeNextFeature )

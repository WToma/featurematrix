module TableView exposing (renderFeatureTable)

import PersistentModel exposing (PersistentModel, Feature, isFeatureVisible)
import Msg exposing (Msg(HideFeature, IntersectionUpdated, FocusFeature))
import Html exposing (Html, button, div, text, textarea, h2)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly)
import Dict exposing (Dict)


renderFeatureTable : PersistentModel -> Maybe String -> Html Msg
renderFeatureTable model parseError =
    div [ class "featureTableContainer" ]
        [ div [] [ text (Maybe.withDefault "" parseError) ]
        , renderFeatureTableGeneric (renderIntersectionEditBox model.intersections) model
        ]


renderFeatureTableGeneric : (( Feature, Feature ) -> Html Msg) -> PersistentModel -> Html Msg
renderFeatureTableGeneric intersectionRenderer model =
    let
        featuresToRender =
            List.filter (isFeatureVisible model) model.features

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
    div []
        [ text f.displayName
        , button [ onClick (HideFeature f.featureId), class "hideBtn" ] [ text "(hide)" ]
        ]


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

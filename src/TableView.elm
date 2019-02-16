module TableView exposing (renderFeatureTable)

import PersistentModel exposing (PersistentModel, Feature, isFeatureVisible)
import Msg exposing (Msg(HideFeature, IntersectionUpdated, FocusFeature))
import Html exposing (Html, button, div, text, textarea, h5)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, attribute)
import Dict exposing (Dict)
import Helpers exposing (flattenMaybeList)


renderFeatureTable : PersistentModel -> Maybe String -> Html Msg
renderFeatureTable model parseError =
    let
        maybeErrorAlert =
            Maybe.map renderParseErrorAlert parseError

        content =
            [ maybeErrorAlert
            , Just (renderFeatureTableGeneric (renderIntersectionEditBox model.intersections) model)
            ]
                |> flattenMaybeList
    in
        div [ class "featureTableContainer" ] content


renderParseErrorAlert : String -> Html msg
renderParseErrorAlert parseError =
    div [ class "alert alert-danger" ] [ text parseError ]


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
                [ Html.table [ class "featureTable table table-bordered" ]
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
    div [ class "card cardNoBorder" ]
        [ div [ class "card-body" ]
            [ h5 [ class "featureName card-title" ] [ text f.displayName ]
            , div [ class "btn-group", attribute "role" "group", attribute "aria-label" ("Actions on the " ++ f.displayName ++ " feature") ]
                [ button [ class "hideBtn btn btn-primary btn-sm", onClick (HideFeature f.featureId) ] [ text "Hide" ]
                , button [ class "focusBtn btn btn-primary btn-sm", onClick (FocusFeature f.featureId) ] [ text "Focus" ]
                ]
            ]
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
        Html.textarea [ class "intersectionTextArea form-control", id inputId, value intersectionValue, onInput (IntersectionUpdated smallerId largerId) ] []

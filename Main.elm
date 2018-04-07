module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value)
import Dict exposing (Dict)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = { intersections = Dict.empty }, view = view, update = update }

type alias Model = {
    intersections: Dict (String, String) String
}

type Msg
    = IntersectionUpdated String String String


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
    ]


renderFeatureTable : List Feature -> Html msg
renderFeatureTable = renderFeatureTableGeneric (\_ -> Html.text "")

renderFeatureTableGeneric : ((Feature, Feature) -> Html msg) -> List Feature -> Html msg
renderFeatureTableGeneric intersectionRenderer features =
    Html.table [ class "featureTable" ]
        (-- header
         [ Html.tr [] ([ Html.th [ class "featureTable" ] [] ] ++ List.map (\f -> Html.th [ class "featureTable" ] [ Html.text f.displayName ]) features) ]
            ++ -- rows
               List.map
                (\f ->
                    Html.tr [] ([ Html.th [ class "featureTable" ] [ Html.text f.displayName ] ] ++ List.map (\f2 -> Html.td [ class "featureTable" ] [intersectionRenderer (f, f2)]) features)
                )
                features
        )

renderIntersectionEditBox: Dict (String, String) String -> (Feature, Feature) -> Html Msg
renderIntersectionEditBox intersectionValues (f1, f2) =
    let
        smallerId = if f1.featureId < f2.featureId then f1.featureId else f2.featureId
        largerId = if f1.featureId < f2.featureId then f2.featureId else f1.featureId
        inputId = smallerId ++ "_vs_" ++ largerId
        intersectionValue = Maybe.withDefault "" (Dict.get (smallerId, largerId) intersectionValues)
    in
        Html.input [type_ "text", id inputId, value intersectionValue, onInput (IntersectionUpdated smallerId largerId)] []

update : Msg -> Model -> Model
update msg model =
    case msg of
        IntersectionUpdated smallerId largerId newValue ->
            { intersections = Dict.insert (smallerId, largerId) newValue model.intersections }


view : Model -> Html Msg
view model =
    div [class "featureTableContainer"]
        [ renderFeatureTableGeneric (renderIntersectionEditBox model.intersections) dummyFeatures
        , text (toString model.intersections)]

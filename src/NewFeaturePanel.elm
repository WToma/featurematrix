module NewFeaturePanel exposing (Model, NewFeatureRequest, Msg, initialModel, update, view, setError)

import Html exposing (Html, button, div, text, textarea, h5)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, placeholder)
import Helpers exposing (flattenMaybeList)


type alias Model =
    { shortName : String
    , description : String
    , errorAdding : Maybe String
    }


type alias NewFeatureRequest =
    { shortName : String
    , description : String
    , enterFocusMode : Bool
    }


type Msg
    = ShortNameUpdated String
    | DescriptionUpdated String
    | AddFeature NewFeatureRequest


initialModel : Model
initialModel =
    { shortName = ""
    , description = ""
    , errorAdding = Nothing
    }


setError : String -> Model -> Model
setError newErrorAdding model =
    { model | errorAdding = Just newErrorAdding }


update : (NewFeatureRequest -> a) -> Msg -> Model -> ( Model, Maybe a )
update addMessageGenerator msg model =
    case msg of
        ShortNameUpdated shortName ->
            ( { model | shortName = shortName }, Nothing )

        DescriptionUpdated description ->
            ( { model | description = description }, Nothing )

        AddFeature request ->
            ( model, Just (addMessageGenerator request) )


view : Model -> Html Msg
view model =
    let
        cardBodyFixedElems =
            [ h5 [ class "card-title" ] [ text "Add New Feature" ]
            , div [ class "input-group mb-3" ]
                [ Html.input [ type_ "text", placeholder "Short Name of the New Feature", class "newFeatureName form-control", value model.shortName, onInput ShortNameUpdated ] [] ]
            , div [ class "input-group mb-3" ]
                [ textarea [ class "newFeatureDescription form-control", placeholder "A description of the new feature", value model.description, onInput DescriptionUpdated ] [] ]
            , div [ class "btn-toolbar" ]
                [ div [ class "mb-3 mr-3" ]
                    [ button [ class "btn btn-primary", onClick (AddFeature { shortName = model.shortName, description = model.description, enterFocusMode = False }) ] [ text "Add Feature" ] ]
                , div [ class "mb-3" ]
                    [ button [ class "addFeatureAndFocus btn btn-primary", onClick (AddFeature { shortName = model.shortName, description = model.description, enterFocusMode = True }) ] [ text "Add Feature and Focus" ] ]
                ]
            ]

        cardBody =
            (List.map (\e -> Just e) cardBodyFixedElems)
                ++ [ Maybe.map error model.errorAdding ]
                |> flattenMaybeList
    in
        div [ class "featureAdd card" ]
            [ div [ class "card-body" ] cardBody
            ]


error : String -> Html msg
error reason =
    div [ class "alert alert-danger" ]
        [ text ("Error Adding New Feature: " ++ reason) ]

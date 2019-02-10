module NewFeaturePanel exposing (Model, NewFeatureRequest, Msg, initialModel, update, view, setError)

import Html exposing (Html, button, div, text, textarea, h2)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, type_, id, value, readonly, placeholder)


type alias Model =
    { shortName : String
    , description : String
    , errorAdding : Maybe String
    }


type alias NewFeatureRequest =
    { shortName : String
    , description : String
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


view : (Msg -> msg) -> Model -> Html msg
view msgWrapper model =
    div [ class "featureAdd" ]
        [ h2 [] [ text "Add New Feature" ]
        , Html.input [ type_ "text", placeholder "Short Name of the New Feature", class "newFeatureName", value model.shortName, onInput (ShortNameUpdated >> msgWrapper) ] []
        , textarea [ class "newFeatureDescription", placeholder "A description of the new feature.", value model.description, onInput (DescriptionUpdated >> msgWrapper) ] []
        , button [ onClick (msgWrapper (AddFeature { shortName = model.shortName, description = model.description })) ] [ text "Add Feature" ]
        , text (Maybe.withDefault "" (Maybe.map (\reason -> "Error Adding New Feature: " ++ reason) model.errorAdding))
        ]

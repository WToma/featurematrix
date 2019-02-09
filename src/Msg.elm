module Msg exposing (Msg(..))


type Msg
    = IntersectionUpdated String String String
    | ShowModel
    | HideModel
    | SerializedModelUpdated String
    | NFPShortNameUpdated String
    | NFPDescriptionUpdated String
    | AddNewFeature String String
    | HideFeature String
    | ShowFeature String

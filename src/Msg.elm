module Msg exposing (Msg(..))

import NewFeaturePanel
import FocusMode


type Msg
    = IntersectionUpdated String String String
    | ShowModel
    | HideModel
    | SerializedModelUpdated String
    | AddNewFeature NewFeaturePanel.NewFeatureRequest
    | HideFeature String
    | ShowFeature String
    | NFPMsg NewFeaturePanel.Msg
    | FocusFeature String
    | FocusModeMsg FocusMode.Msg

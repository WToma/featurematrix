module Model exposing (Model, PersistentModel, Feature, encodePersistentModel, decodePersistentModel, isFeatureVisible, addNewFeature)

import NewFeaturePanel
import Dict exposing (Dict)
import Set
import Json.Encode as JE
import Json.Decode as JD
import Helpers exposing (counts, flattenMaybeList, phraseToCamelCase)


type alias PersistentModel =
    { features : List Feature
    , featureVisibility : Dict String Bool
    , intersections : Dict ( String, String ) String
    }


type alias Model =
    { persistent : PersistentModel
    , parseError : Maybe String
    , showSerialized : Bool
    , newFeaturePanelState : NewFeaturePanel.Model
    }


type alias Feature =
    { featureId : String
    , displayName : String
    , description : String
    }



-- helpers


isFeatureVisible : PersistentModel -> Feature -> Bool
isFeatureVisible model feature =
    Dict.get feature.featureId model.featureVisibility |> Maybe.withDefault False


addNewFeature : PersistentModel -> String -> String -> Result String PersistentModel
addNewFeature model newShortName newDescription =
    let
        isShortNameEmpty =
            String.isEmpty newShortName

        isDescriptionEmpty =
            String.isEmpty newDescription

        isShortNameTaken =
            List.any (\f -> f.displayName == newShortName) model.features
    in
        if (not isShortNameTaken) && (not isDescriptionEmpty) && (not isShortNameEmpty) then
            Result.Ok
                (let
                    featureId =
                        getUniqueFeatureId (List.map (\f -> f.featureId) model.features) (phraseToCamelCase newShortName) 0

                    newFeature =
                        { featureId = featureId, displayName = newShortName, description = newDescription }
                 in
                    { model
                        | features = List.append model.features [ newFeature ]
                        , featureVisibility = Dict.insert featureId True model.featureVisibility
                    }
                )
        else
            Result.Err
                (if isShortNameTaken then
                    "a feature with this name already exists"
                 else if isShortNameEmpty then
                    "the feature name cannot be empty"
                 else
                    -- isDescriptonEmpty
                    "the description cannot be empty"
                )



-- to / from JSON


encodePersistentModel : PersistentModel -> String
encodePersistentModel persistentModel =
    JE.encode 4 <|
        JE.object
            [ ( "intersections", encodeIntersections persistentModel.intersections )
            , ( "features", encodeFeatures persistentModel.features )
            , ( "featureVisibilities", encodeFeatureVisibilities persistentModel.featureVisibility )
            ]


decodePersistentModel : String -> Result (List String) PersistentModel
decodePersistentModel str =
    JD.decodeString parsePersistentModel str
        |> Result.mapError List.singleton
        |> Result.andThen validatePersistentModel



-- unexported


encodeFeatures : List Feature -> JE.Value
encodeFeatures features =
    JE.list (List.map encodeFeature features)


encodeFeature : Feature -> JE.Value
encodeFeature feature =
    JE.object
        [ ( "featureId", JE.string feature.featureId )
        , ( "displayName", JE.string feature.displayName )
        , ( "description", JE.string feature.description )
        ]


encodeIntersections : Dict ( String, String ) String -> JE.Value
encodeIntersections intersectionValues =
    JE.list (List.map encodeIntersectionEntry (Dict.toList intersectionValues))


encodeIntersectionEntry : ( ( String, String ), String ) -> JE.Value
encodeIntersectionEntry ( ( smallerKey, largerKey ), value ) =
    JE.object
        [ ( "smallerKey", JE.string smallerKey )
        , ( "largerKey", JE.string largerKey )
        , ( "value", JE.string value )
        ]


encodeFeatureVisibilities : Dict String Bool -> JE.Value
encodeFeatureVisibilities featureVisibilities =
    let
        encodeEntry ( featureId, visible ) =
            JE.object [ ( "featureId", JE.string featureId ), ( "visible", JE.bool visible ) ]
    in
        JE.list (List.map encodeEntry (Dict.toList featureVisibilities))


{-| Returns the model if the model is fine, or the messages describing the problems otherwise
-}
validatePersistentModel : PersistentModel -> Result (List String) PersistentModel
validatePersistentModel persistentModel =
    let
        nonUniqueEntries xs =
            counts xs |> Dict.filter (\_ count -> count > 1) |> Dict.toList |> List.map Tuple.first

        reportNonUniqueEntries desc xs =
            let
                nonUnique =
                    nonUniqueEntries xs
            in
                if List.isEmpty nonUnique then
                    Nothing
                else
                    Just (desc ++ ": " ++ (String.join ", " nonUnique))

        validFeatureIds =
            List.map .featureId persistentModel.features

        reportInvalidFeatureId desc id =
            if List.member id validFeatureIds then
                Nothing
            else
                Just (desc ++ ": " ++ id)

        featuresProblems =
            [ reportNonUniqueEntries "The following feature IDs are duplicated" (List.map .featureId persistentModel.features)
            , reportNonUniqueEntries "The following feature names are duplicated" (List.map .displayName persistentModel.features)
            ]

        visibilityProblems =
            persistentModel.featureVisibility
                |> Dict.toList
                |> List.map Tuple.first
                |> List.map (reportInvalidFeatureId "Invalid feature ID in the visibility list")

        keysInIntersections =
            Set.union
                (persistentModel.intersections |> Dict.toList |> List.map Tuple.first |> List.map Tuple.first |> Set.fromList)
                (persistentModel.intersections |> Dict.toList |> List.map Tuple.first |> List.map Tuple.second |> Set.fromList)
                |> Set.toList

        intersectionProblems =
            keysInIntersections
                |> List.map (reportInvalidFeatureId "Invalid feature ID in the intersection map")

        allProblems =
            List.concat [ featuresProblems, visibilityProblems, intersectionProblems ] |> flattenMaybeList
    in
        if List.isEmpty allProblems then
            Result.Ok persistentModel
        else
            Result.Err allProblems


parsePersistentModel : JD.Decoder PersistentModel
parsePersistentModel =
    JD.map3 PersistentModel
        (JD.field "features" parseFeatures)
        (JD.field "featureVisibilities" parseFeatureVisibilities)
        (JD.field "intersections" parseIntersections)


parseFeatures : JD.Decoder (List Feature)
parseFeatures =
    JD.list <|
        JD.map3 Feature
            (JD.field "featureId" JD.string)
            (JD.field "displayName" JD.string)
            (JD.field "description" JD.string)


parseFeatureVisibilities : JD.Decoder (Dict String Bool)
parseFeatureVisibilities =
    JD.list (JD.map2 (,) (JD.field "featureId" JD.string) (JD.field "visible" JD.bool))
        |> JD.map Dict.fromList


parseIntersections : JD.Decoder (Dict ( String, String ) String)
parseIntersections =
    JD.map Dict.fromList (JD.list parseIntersectionEntry)


parseIntersectionEntry : JD.Decoder ( ( String, String ), String )
parseIntersectionEntry =
    let
        mkEntry smallerKey largerKey value =
            ( ( smallerKey, largerKey ), value )

        smallerKeyDec =
            JD.field "smallerKey" JD.string

        largerKeyDec =
            JD.field "largerKey" JD.string

        valueDec =
            JD.field "value" JD.string
    in
        JD.map3 mkEntry smallerKeyDec largerKeyDec valueDec


getUniqueFeatureId : List String -> String -> Int -> String
getUniqueFeatureId takens base try =
    let
        suffix =
            if try == 0 then
                ""
            else
                toString try

        candidate =
            base ++ suffix

        taken =
            List.any (\x -> x == candidate) takens
    in
        if taken then
            getUniqueFeatureId takens base (try + 1)
        else
            candidate

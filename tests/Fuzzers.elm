module Fuzzers exposing (..)

import Fuzz
import Main
import Dict
import Helpers exposing (..)
import Random.Pcg as Random exposing (..)
import Char
import Maybe
import Tuple
import Shrink
import Lazy.List as LazyList
import DebugTime

modelFuzzerAllVisible : Fuzz.Fuzzer Main.Model
modelFuzzerAllVisible =
    Fuzz.custom modelGeneratorAllVisible modelShrinker


fromInt : Int -> Main.Model
fromInt seedSource =
    let
        seed = initialSeed seedSource
        (model, _) = step modelGeneratorAllVisible seed
    in
        model


shortNonEmptyList : Generator a -> Generator (List a)
shortNonEmptyList g =
    int 1 10
        |> andThen ((flip list) g)

randomLengthList : Int -> Generator a -> Generator (List a)
randomLengthList maxLength g =
    int 1 maxLength
        |> andThen ((flip list) g)
    

textLike : Generator String
textLike = nonEmptyString --shortNonEmptyList nonEmptyString |> map (String.join " ") -- the commented implementation is about 8x slower


featureGenerator : Generator Main.Feature
featureGenerator =
    let
        displayName = textLike
        featureId = nonEmptyString
        description = textLike

        mkFeature displayName featureId description =
            { featureId = featureId, displayName = displayName, description = description }
    in
        map3 mkFeature displayName featureId description


intersectionGenerator : List String -> Generator (Dict.Dict ( String, String ) String)
intersectionGenerator featureIds =
    let
        featureId1 =
            sample featureIds

        featureId2 =
            sample featureIds

        featureIdPairUnordered : Generator (Maybe (String, String))
        featureIdPairUnordered = 
            map2 (Maybe.map2 (,)) featureId1 featureId2

        featureIdPair : Generator (Maybe (String, String))
        featureIdPair =
            featureIdPairUnordered
                |> map (Maybe.map orderTuple)

        intersectionValue = textLike

        mkIntersection : Maybe (String, String) -> String -> Maybe ((String, String), String)
        mkIntersection maybeFeatureId value =
            Maybe.map (\featureId -> (featureId, value)) maybeFeatureId

        intersection : Generator (Maybe ((String, String), String))
        intersection =
            map2 mkIntersection featureIdPair intersectionValue

        numFeatures = List.length featureIds
        numMeaningfulFeatures = ((numFeatures ^ 2) - numFeatures) // 2 - numFeatures

        intersections : Generator (List ((String, String), String))
        intersections =
            randomLengthList numMeaningfulFeatures intersection |> map flattenMaybeList
    in
        map Dict.fromList intersections


modelGeneratorAllVisible : Generator Main.Model
modelGeneratorAllVisible =
    let
        features =
            randomLengthList 10 featureGenerator

        featureIds =
            map (List.map (\f -> f.featureId)) features

        intersections =
            featureIds
                |> andThen intersectionGenerator
    in
        map2
            (\features intersections ->
                { features = features
                , featureVisibility = List.map (\f -> ( f.featureId, True )) features |> Dict.fromList
                , intersections = intersections
                , parseError = Nothing
                , showSerialized = False
                , newFeaturePanelState =
                    { shortName = ""
                    , description = ""
                    , errorAdding = Nothing
                    }
                }
            )
            features
            intersections


modelShrinker : Shrink.Shrinker Main.Model
modelShrinker model =
    let
        ensureGoodShrink = LazyList.dropIf (\m -> m == model)
    in
        LazyList.map (\s -> s model) featureDropShrinkStrategies |> ensureGoodShrink


featureDropShrinkStrategies : LazyList.LazyList (Main.Model -> Main.Model)
featureDropShrinkStrategies = LazyList.fromList
    [ DebugTime.printElapsedTime1 "dropEmptyFeatures" dropEmptyFeatures
    , DebugTime.printElapsedTime1 "dropNonEmptyFeatures" dropNonEmptyFeatures
    , DebugTime.printElapsedTime1 "dropOddFeatures" dropOddFeatures
    , DebugTime.printElapsedTime1 "dropEvenFeatures" dropEvenFeatures
    , DebugTime.printElapsedTime1 "dropFirstHalfOfFeatures" dropFirstHalfOfFeatures
    , DebugTime.printElapsedTime1 "dropSecondHalfOfFeatures" dropSecondHalfOfFeatures
    , DebugTime.printElapsedTime1 "dropFirstAndLastQuarterOfFeatures" dropFirstAndLastQuarterOfFeatures
    ]


    

isFeatureIdEmpty : Dict.Dict (String, String) String -> String -> Bool
isFeatureIdEmpty intersections featureId =
    let
        keys = Dict.keys intersections
        keyContains key = (Tuple.first key == featureId || Tuple.second key == featureId)
        keysContainingFeature = List.filter keyContains keys
    in
        List.isEmpty keysContainingFeature

dropFeaturesFromIntersections : List String -> Dict.Dict (String, String) String -> Dict.Dict (String, String) String
dropFeaturesFromIntersections featureIdsToDrop intersections =
    let
        keyContains featureId key = (Tuple.first key == featureId || Tuple.second key == featureId)
        removeFeatureId featureId intersections = Dict.filter (\key _ -> not <| keyContains featureId key) intersections
    in        
        List.foldl removeFeatureId intersections featureIdsToDrop

dropFeaturesFromFeatureList : List String -> List Main.Feature -> List Main.Feature
dropFeaturesFromFeatureList featureIdsToDrop features =
    List.filter (\feature -> not (List.member feature.featureId featureIdsToDrop)) features

dropFeatureFromVisibilities : List String -> Dict.Dict String Bool -> Dict.Dict String Bool
dropFeatureFromVisibilities featureIdsToDrop visibilities =
    Dict.filter (\featureId _ -> not (List.member featureId featureIdsToDrop)) visibilities

-- removes the given featureIds and all references to it from the model
dropFeatures : List String -> Main.Model -> Main.Model
dropFeatures featureIdsToDrop model =
    let
        newFeatures = dropFeaturesFromFeatureList featureIdsToDrop model.features
        newIntersections = dropFeaturesFromIntersections featureIdsToDrop model.intersections
        newVisibilities = dropFeatureFromVisibilities featureIdsToDrop model.featureVisibility
    in
        {model | features = newFeatures, intersections = newIntersections, featureVisibility = newVisibilities}


dropFeaturesBy : (String -> Bool) -> Main.Model -> Main.Model
dropFeaturesBy predicate model =
    let
        featureIdsToDrop = List.filter predicate (List.map (\f -> f.featureId) model.features)
    in
        dropFeatures featureIdsToDrop model


-- shrinking strategy: drops all features with no intersections
dropEmptyFeatures : Main.Model -> Main.Model
dropEmptyFeatures model = dropFeaturesBy (isFeatureIdEmpty model.intersections) model

--shrinking strategy: drops all features with intersections
dropNonEmptyFeatures : Main.Model -> Main.Model
dropNonEmptyFeatures model = dropFeaturesBy (not << (isFeatureIdEmpty model.intersections)) model
        

-- drops the features whose index in the features list makes the predicate return true (the second arg to the predicate
-- is the number of features)
dropFeaturesByIndex : (Int -> Int -> Bool) -> Main.Model -> Main.Model
dropFeaturesByIndex indexPredicate model =
    let
        numFeatures = List.length model.features
        indices = List.range 0 (numFeatures - 1)
        indicesToDrop = List.filter (\i -> indexPredicate i numFeatures) indices
        featureIdsToDrop = List.map ((flip listElemAtIndex) model.features) indicesToDrop
            |> flattenMaybeList
            |> List.map (\f -> f.featureId)
    in
        dropFeatures featureIdsToDrop model

dropEvenFeatures : Main.Model -> Main.Model
dropEvenFeatures = dropFeaturesByIndex (\i _ -> i % 2 == 0)

dropOddFeatures : Main.Model -> Main.Model
dropOddFeatures = dropFeaturesByIndex (\i _ -> i % 2 /= 0)

dropFirstHalfOfFeatures : Main.Model -> Main.Model
dropFirstHalfOfFeatures = dropFeaturesByIndex (\i n -> i <= n // 2)

dropSecondHalfOfFeatures : Main.Model -> Main.Model
dropSecondHalfOfFeatures = dropFeaturesByIndex (\i n -> i > n // 2)

dropFirstAndLastQuarterOfFeatures : Main.Model -> Main.Model
dropFirstAndLastQuarterOfFeatures = dropFeaturesByIndex (\i n -> i <= n // 4 || i > n // 4)



-- copy of Fuzz.string, except it won't generate empty strings
nonEmptyString : Generator String
nonEmptyString =
    let
        asciiGenerator : Generator String
        asciiGenerator =
            Random.frequency
                [ ( 3, Random.int 1 10 )
                , ( 1, Random.int 11 50 )
                , ( 1, Random.int 50 1000 )
                ]
                |> Random.andThen (lengthString asciiCharGenerator)

        whitespaceGenerator : Generator String
        whitespaceGenerator =
            Random.int 1 10
                |> Random.andThen (lengthString whitespaceCharGenerator)
    in
        Random.frequency
            [ ( 9, asciiGenerator )
            , ( 1, whitespaceGenerator )
            ]


lengthString : Generator Char -> Int -> Generator String
lengthString charGenerator stringLength =
    list stringLength charGenerator
        |> map String.fromList


asciiCharGenerator : Generator Char
asciiCharGenerator =
    Random.map Char.fromCode (Random.int 32 126)


whitespaceCharGenerator : Generator Char
whitespaceCharGenerator =
    Random.sample [ ' ', '\t', '\n' ] |> Random.map (Maybe.withDefault ' ')

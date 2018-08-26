module GeneratorPerfTest exposing (generatorPerfSuite)

import Test exposing (Test, describe, todo, test, fuzz, fuzz3)
import Expect
import Random.Pcg as Random
import Fuzzers
import Debug
import Dict

generatorPerfSuite : Test
generatorPerfSuite = describe "generator performance"
    [ 
        test "generating ints" <|
            \() ->
                let
                    numInts = 100000 -- 50k = 324 ms; 100k = 390ms
                    maxInt = 100
                    value = sumRandomIntsRecursive numInts maxInt
                in
                    Expect.atMost (numInts * maxInt) value,
        test "generating ints with andThen" <|
            \() ->
                let
                    value = chainedRandomGenerators 50000
                in
                    Expect.atMost Random.maxInt value,

        Test.only <| test "generating textLikes" <|
            \() ->
                let
                    -- original impl: 1000 = 1099ms; 10000 = 9616ms --> 0.9616ms / textLike
                    -- same as "nonEmptyString": 10000 = 1887ms -> 0.1887ms / nonEmptyString
                    value = generateAndCombine 10000 Fuzzers.textLike (\s a -> a + (String.length s)) 0
                in
                    Expect.atLeast 0 value,

        test "generating features" <|
            \() ->
                let -- 1000 = 2228ms; 10000 = 19433ms; 1.9433ms / feature
                    value = generateAndCombine 10000 Fuzzers.featureGenerator (\f a -> a + (String.length f.description)) 0
                in
                    Expect.atLeast 0 value,

        test "generating features for a model (50)" <|
            \() ->
                let -- 100 (2515) = 5025ms; 1000 (25615) = 51603ms --> 51ms / model, 2.015ms / feature
                    max50List = Fuzzers.randomLengthList 50 Fuzzers.featureGenerator
                    value = generateAndCombine 1000 max50List (\fs a -> a + (List.length fs)) 0
                    _ = Debug.log "total number of feature generated" value
                in                    
                    Expect.atLeast 0 value,

        test "generating intersections (25 features)" <|
            \() ->
                let -- 100 (9663) = 11824ms; 200 (20636) = 24590ms --> 122ms / model, 1.191ms / intersection (textLike)
                    featureIds = List.range 1 25 |> List.map (\i -> "feature" ++ (toString i))
                    value = generateAndCombine 200 (Fuzzers.intersectionGenerator featureIds) (\i a -> a + (Dict.size i)) 0
                    _ = Debug.log "total number of intersections generated" value                    
                in
                    Expect.atLeast 0 value,

        -- from the above 2: features for model + intersections = average 173ms

        test "generating full models" <|
            \() ->
                let
                    -- 100 (2620 features, 15186 intersections) 28254ms; 200 (5104 features, 29653 intersections) 54930ms --> 274.65ms / model
                    -- there is a 100ms overhead compared to just raw features + intersections, even though it still comes out to an average 25
                    -- features!
                    gen = Fuzzers.modelGeneratorAllVisible

                    -- gen =
                    --     let
                    --         features =
                    --             Fuzzers.randomLengthList 50 Fuzzers.featureGenerator

                    --         -- featureIds = Random.constant (List.range 1 25 |> List.map (\i -> "feature" ++ (toString i)))
                    --         -- if the feature ids are mapped from the actual features -> above result
                    --         -- if the feature ids are constant 25 length -> 17389ms (for 100 / 2783 features / 10104 intersections)
                    --         -- if the feature ids are a random list 0-50 length -> 2532ms (for 100 / 2551 features / 16761 intersections)
                    --         featureIds =
                    --             Random.int 1 50
                    --                 |> Random.map (List.range 1)
                    --                 |> Random.map (List.map (\i -> "feature" ++ (toString i)))

                    --         intersections =
                    --             featureIds
                    --                 |> Random.andThen Fuzzers.intersectionGenerator
                    --     in
                    --         Random.map2 (\features intersections -> { features = features, intersections = intersections}) features intersections

                    combiner model (totalFeatures, totalIntersections) =
                        (totalFeatures + (List.length model.features), totalIntersections + (Dict.size model.intersections))                            
                    (totalFeatures, totalIntersections) = generateAndCombine 100 gen combiner (0, 0)
                    _ = Debug.log "total number of features generated" totalFeatures
                    _ = Debug.log "total number of intersections generated" totalIntersections
                in
                    Expect.atLeast 0 totalFeatures
    ]

generateAndCombine : Int -> Random.Generator a -> (a -> b -> b) -> b -> b
generateAndCombine num gen combine acc0 =
    let
        initialSeed = Random.initialSeed 42
        generateRecursive seed remaining acc =
            let
                (value, newSeed) = Random.step gen seed
                newRemaining = remaining - 1
                newAcc = combine value acc
            in
                if newRemaining > 0 then
                    generateRecursive newSeed newRemaining newAcc
                else
                    newAcc
    in
        generateRecursive initialSeed num acc0

sumRandomIntsRecursive : Int -> Int -> Int
sumRandomIntsRecursive numInts maxRandom =
    let
        generator = Random.int 0 maxRandom
        initialSeed = Random.initialSeed 42
        generateRecursive seed remaining sum =
            let
                (value, newSeed) = Random.step generator seed
                newRemaining = remaining - 1
                newSum = sum + value
            in
                if newRemaining > 0 then
                    generateRecursive newSeed newRemaining newSum 
                else
                    newSum
    in
        generateRecursive initialSeed numInts 0

chainedGenerator : Random.Generator Int -> Random.Generator Int
chainedGenerator gen = gen |> Random.andThen (\limit ->
    if limit > 0 then 
        Random.int 0 limit
    else
        Random.int 0 Random.maxInt)


chainedRandomGenerators : Int -> Int
chainedRandomGenerators numInts =
    let
        chainRecursive previousGen remaining =
            if remaining > 0 then
                chainRecursive (chainedGenerator previousGen) (remaining - 1)
            else
                previousGen
        finalChainedGenerator = chainRecursive (Random.int 0 Random.maxInt) numInts
        initialSeed = Random.initialSeed 42
        (value, _) = Random.step finalChainedGenerator initialSeed
    in
        value
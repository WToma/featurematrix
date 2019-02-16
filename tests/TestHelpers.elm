module TestHelpers exposing (resultToExpectation, expectNotError, dummyIntersections, initialModel)

import Expect exposing (Expectation)
import Dict
import Main


resultToExpectation : Result String Expectation -> Expectation
resultToExpectation res =
    case res of
        Err reason ->
            Expect.fail reason

        Ok expectation ->
            expectation


expectNotError : Result String a -> Expectation
expectNotError r =
    case r of
        Result.Ok _ ->
            Expect.pass

        Result.Err msg ->
            Expect.fail msg


dummyIntersections : Dict.Dict ( String, String ) String
dummyIntersections =
    Dict.fromList
        [ ( ( "edit", "showFeatures" ), "edited features are displayed" )
        , ( ( "edit", "export" ), "edited features are exported" )
        , ( ( "addFeature", "import" ), "added features can be exported and imported" )
        ]


initialModel : Main.Model
initialModel =
    { persistent =
        { features = Main.dummyFeatures
        , featureVisibility = Main.allFeaturesVisible Main.dummyFeatures
        , intersections = dummyIntersections
        }
    , parseError = Nothing
    , showSerialized = False
    , newFeaturePanelState =
        { shortName = ""
        , description = ""
        , errorAdding = Nothing
        }
    , display = Main.Table
    }

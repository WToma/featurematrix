module TestHelpers exposing (resultToExpectation, expectNotError)

import Expect exposing (Expectation)


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

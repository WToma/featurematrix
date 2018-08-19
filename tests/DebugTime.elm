module DebugTime exposing (printElapsedTime, printElapsedTime1)

import Native.GetCurrentTimeMillis
import Debug

printElapsedTime : String -> (() -> a) -> () -> a
printElapsedTime name f =
    \() ->
        let
            startTime = Native.GetCurrentTimeMillis.getCurrentTimeMillis()
            result = f()
            endTime = Native.GetCurrentTimeMillis.getCurrentTimeMillis()
            _ = Debug.log ((toString (endTime - startTime)) ++ "ms") name
        in
            result

printElapsedTime1 : String -> (a -> b) -> (a -> b)
printElapsedTime1 name f =
    \x -> (printElapsedTime name (\() -> f x))()
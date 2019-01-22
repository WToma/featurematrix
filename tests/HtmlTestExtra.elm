module HtmlTestExtra exposing (fromHtml, extractText, simulate, Attributes, getStringAttribute, getBoolAttribute, getAttributes)

{-| Copy of Inert.elm from <https://raw.githubusercontent.com/eeue56/elm-html-test/5.2.0/src/Html/Inert.elm>
Duplicating it here because elm-html-test currently doesn't support certain testing scenarios.

    In my use case, I have a <table> with some <tr> rows, each row (except the first one) has a <th> header.
    I'd like to verify that the texts in those <th> headers match some expected values; however the headers can be in any order
    (i.e. I don't want to make it a requirement that they appear in a certain order). This means I can't use `Query.index` to
    match up each row header with the expected value. We somehow need to get _all the row header values_ as a `Set` and compare
    against the `Set` of expected values.

    This file focuses on satisfying that use case, but I'll try not to make it specific to my program.

-}

import ElmHtml.InternalTypes exposing (ElmHtml(..), EventHandler, Facts, Tagger, decodeElmHtml)
import Html exposing (Html)
import Json.Decode
import Native.HtmlAsJson
import Dict
import Json.Encode


{-| Convert a html into a queryable (using elm-html-query) format. You can feed any elm-generated HTML into this, e.g.
from your view function

    fromHtml div [] [h1 [] [text "Hello World!"]]

-}
fromHtml : Html msg -> ElmHtml msg
fromHtml html =
    case Json.Decode.decodeValue (decodeElmHtml taggedEventDecoder) (toJson html) of
        Ok elmHtml ->
            elmHtml

        Err str ->
            Debug.crash ("Error internally processing HTML for testing - please report this error message as a bug: " ++ str)


{-| Extracts the text from the given html node. This can be a text node itself, or a node containing text directly
(e.g. <h1>Header!</h1>). However if the child is deeper than that no text will be returned.
Note: I don't think it's valid to have multiple text nodes inside a Node, which is the only case this function would return
multiple results.

    extractText (fromHtml (Html.text "hello")) == ["hello"]
    extractText (fromHtml (Html.h1 [] [Html.text "hello"])) == ["hello"]
    extractText (fromHtml (Html.h1 [] [Html.text "hello", Html.text "world"])) == ["hello", "world"]
    extractText (fromHtml (Html.div [] [Html.h1 [] [Html.text "Hello World!"]])) == []

-}
extractText : ElmHtml msg -> List String
extractText elmHtml =
    extractTextDescend elmHtml 1


extractTextDescend : ElmHtml msg -> Int -> List String
extractTextDescend elmHtml remainingDepth =
    case elmHtml of
        TextTag { text } ->
            [ text ]

        NodeEntry { children } ->
            if remainingDepth > 0 then
                List.foldr (\child texts -> List.append texts (extractTextDescend child (remainingDepth - 1))) [] children
            else
                []

        _ ->
            []


{-| Simulates the given event on the given node, and returns the generated message, if any, or an error. To generate
events to be used with the simulation, use elm-html-test's Event functions.

    let
        html = fromHtml (Html.input [Html.Attribute.type "text", Html.Events.onInput TextEntered])
        event = Test.Html.Event.input "hello, world"
    in
        simulate event html == TextEntered "hello, world"

-}
simulate : ( String, Json.Encode.Value ) -> ElmHtml msg -> Result String msg
simulate ( eventName, jsEvent ) html =
    findEvent eventName html
        |> Result.andThen (\foundEvent -> Json.Decode.decodeValue foundEvent jsEvent)


findEvent : String -> ElmHtml msg -> Result String (Json.Decode.Decoder msg)
findEvent eventName element =
    let
        eventDecoder node =
            node.facts.events
                |> Dict.get eventName
                |> Result.fromMaybe ("Event.expectEvent: The event " ++ eventName ++ " does not exist on the node.")
    in
        case element of
            TextTag { text } ->
                Err ("Found element is a text, which does not produce events, therefore could not simulate " ++ eventName ++ " on it. Text found: " ++ text)

            NodeEntry node ->
                eventDecoder node

            CustomNode node ->
                eventDecoder node

            MarkdownNode node ->
                eventDecoder node

            NoOp ->
                Err ("Unknown element found. Could not simulate " ++ eventName ++ " on it.")


{-| Convert a Html node to a Json string
-}
toJson : Html a -> Json.Decode.Value
toJson node =
    Native.HtmlAsJson.toJson node


{-| Gets the function out of a tagger
-}
taggerFunction : Tagger -> (a -> msg)
taggerFunction tagger =
    Native.HtmlAsJson.taggerFunction tagger


{-| Gets the decoder out of an EventHandler
-}
eventDecoder : EventHandler -> Json.Decode.Decoder msg
eventDecoder eventHandler =
    Native.HtmlAsJson.eventDecoder eventHandler


{-| Applies the taggers over the event handlers to have the complete event decoder
-}
taggedEventDecoder : List Tagger -> EventHandler -> Json.Decode.Decoder msg
taggedEventDecoder taggers eventHandler =
    case taggers of
        [] ->
            eventDecoder eventHandler

        [ tagger ] ->
            Json.Decode.map (taggerFunction tagger) (eventDecoder eventHandler)

        tagger :: taggers ->
            Json.Decode.map (taggerFunction tagger) (taggedEventDecoder taggers eventHandler)


type alias Attributes =
    { stringAttributes : Dict.Dict String String
    , boolAttributes : Dict.Dict String Bool
    }


getStringAttribute : String -> Attributes -> Maybe String
getStringAttribute k m =
    Dict.get k m.stringAttributes


getBoolAttribute : String -> Attributes -> Maybe Bool
getBoolAttribute k m =
    Dict.get k m.boolAttributes


{-| Returns the string attributes and bool attributes of the given node. If the given HTML is not a node
empty dictionaries are returned.
-}
getAttributes : ElmHtml msg -> Attributes
getAttributes elmHtml =
    case elmHtml of
        NodeEntry { facts } ->
            Attributes facts.stringAttributes facts.boolAttributes

        _ ->
            Attributes Dict.empty Dict.empty

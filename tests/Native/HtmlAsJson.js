/**
 * Copy of https://raw.githubusercontent.com/eeue56/elm-html-test/5.2.0/src/Native/HtmlAsJson.js
 * To go along with HtmlTestExtra.elm -- see notes there.
 */

var _user$project$Native_HtmlAsJson = (function() {
    function forceThunks(vNode) {
        if (typeof vNode !== "undefined" && vNode.ctor === "_Tuple2" && !vNode.node) {
            vNode._1 = forceThunks(vNode._1);
        }
        if (typeof vNode !== 'undefined' && vNode.type === 'thunk' && !vNode.node) {
            vNode.node = vNode.thunk.apply(vNode.thunk, vNode.args);
        }
        if (typeof vNode !== 'undefined' && vNode.type === 'tagger') {
            vNode.node = forceThunks(vNode.node);
        }
        if (typeof vNode !== 'undefined' && typeof vNode.children !== 'undefined') {
            vNode.children = vNode.children.map(forceThunks);
        }
        return vNode;
    }

    return {
        toJson: function(html) {
            return forceThunks(html);
        },
        eventDecoder: function (event) {
            return event.decoder;
        },
        taggerFunction: function (tagger) {
            return tagger;
        },
        attributeToJson: function(attribute) {
          return attribute;
        }
    };
})();

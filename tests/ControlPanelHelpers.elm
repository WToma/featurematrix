module ControlPanelHelpers exposing (findShowHideModelButton, findExportImportTextField)

import ElmHtml.InternalTypes exposing (ElmHtml)
import ElmHtml.Query exposing (queryByTagName, queryByClassName, queryById)
import Helpers exposing (..)


findShowHideModelButton : ElmHtml msg -> Maybe (ElmHtml msg)
findShowHideModelButton html =
    queryById "hideShowSaveLoadBox" html |> ensureSingleton


findExportImportTextField : ElmHtml msg -> Maybe (ElmHtml msg)
findExportImportTextField html =
    queryByClassName "saveLoadBox" html |> ensureSingleton

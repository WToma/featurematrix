module FocusModeTests exposing (..)

import Test exposing (Test, describe, todo, test)


entering : Test
entering =
    describe "To enter focus mode"
        [ todo "there is a button on the feature headers in table view"
        , todo "there is a button on the add feature panel"
        ]


focusMode : Test
focusMode =
    describe "In Focus Mode"
        [ todo "there is no feature table"
        , todo "one intersection is shown at a time"
        , todo "that intersection can be edited"
        , todo "there are buttons to go to the previous / next feature, the other feature for the intersection is fixed"
        , todo "there is a button to return to table view"
        , todo "export works the same way"
        , todo "if a new feature is added, the new feature can be reached while cycling through the features"
        , todo "hidden features still show up"
        ]


importing : Test
importing =
    describe "When importing in Focus Mode"
        [ todo "if the import changes the currently focused intersection, it's shown immediately"
        , todo "if the focused feature no longer exists, the view returns to table view"
        , todo "if the other feature currently shown no longer exists, we go to the next feature that exists"
        , todo "if the import text is invalid, an error message is shown (the same as in table view)"
        ]

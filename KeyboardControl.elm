module KeyboardControl exposing (..)

import Model exposing (..)
import Settings


handleKeyPress : Char -> Scene -> Scene
handleKeyPress char model =
    case char of
        '-' ->
            speedUp model

        '+' ->
            slowDown model

        _ ->
            let
                newSettings =
                    case char of
                        ' ' ->
                            Settings.update (Settings.SetAll False) model.settings

                        'l' ->
                            Settings.update (Settings.SetAll True) model.settings

                        _ ->
                            Settings.update (Settings.ToggleSetting char) model.settings
            in
                { model | settings = newSettings }


handleArrowKeys : Keys -> Scene -> Scene
handleArrowKeys keys model =
    model



-------------IMPL


speedUp : Scene -> Scene
speedUp model =
    { model | speedMult = max (1 / 32) (model.speedMult / 2) }


slowDown : Scene -> Scene
slowDown model =
    { model | speedMult = min 32 (model.speedMult * 2) }

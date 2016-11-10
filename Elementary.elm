module Elementary exposing (..)

import Element as El exposing (..)
import Collage as Co exposing (..)
import Color exposing (Color)


betterCollage : Float -> Float -> List Form -> Element
betterCollage w h list =
    collage (round w) (round h) (list ++ [ filled Color.red (circle 2) ])


emptyForm : Form
emptyForm =
    Co.toForm empty

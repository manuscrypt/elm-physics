module BoundingOval exposing (..)

import Html as Html exposing (..)
import Collage as Co exposing (..)
import Element as El exposing (..)
import Color exposing (Color)
import Model exposing (..)
import Drawing exposing (..)
import Vector2 exposing (..)
import VectorAlgebra exposing (..)
import Elementary exposing (betterCollage)


forBody : Body -> Oval
forBody body =
    let
        ( orig, radii ) =
            calculate body.shape
    in
        ( orig <+> body.pos, radii )


calculate : BodyShape -> Oval
calculate s =
    let
        root2 num =
            num * (sqrt 2)
    in
        case s of
            Circle r ->
                ovalFromTuples ( 0, 0 ) ( 2 * r, 2 * r )

            Square r ->
                let
                    ro =
                        root2 r
                in
                    ovalFromTuples ( 0, 0 ) ( ro, ro )

            Rectangle x y ->
                ovalFromTuples ( 0, 0 ) ( root2 x, root2 y )

            NGon r i ->
                ovalFromTuples ( 0, 0 ) ( 2 * r, 2 * r )


test : BodyShape -> Color -> ( Float, Float ) -> Form
test s c o =
    move o <| group [ drawShape s c, drawOval (calculate s) (dashed c) ]


main : Html a
main =
    toHtml
        <| betterCollage 1200 600
        <| List.map3 test
            [ Circle 50, Square 50, Rectangle 50 100, NGon 50 3, NGon 50 4 ]
            [ Color.black, Color.blue, Color.yellow, Color.purple, Color.brown ]
            [ ( 0, 0 ), ( 110, 0 ), ( -90, 0 ), ( 210, 0 ), ( 310, 0 ) ]

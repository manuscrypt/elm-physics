module BoundingRect exposing (..)

import Collage as Co exposing (..)
import Color exposing (Color)
import Model exposing (..)
import Vector2 exposing (Vector, (<+>), fromTuple)
import Maybe exposing (withDefault)
import List exposing (map, minimum, maximum)
import Drawing exposing (drawShape, drawRect)
import VectorAlgebra exposing (rectFromTuples)


forBody : Body -> Rect
forBody body =
    let
        ( orig, a, b ) =
            calculate body.shape
    in
        ( orig <+> body.pos, a, b )


calculate : BodyShape -> Rect
calculate s =
    case s of
        Circle r ->
            rectFromTuples ( -r, -r ) ( 0, 2 * r ) ( 2 * r, 0 )

        Square r ->
            rectFromTuples ( -r / 2, -r / 2 ) ( 0, r ) ( r, 0 )

        Rectangle x y ->
            rectFromTuples ( -x / 2, -y / 2 ) ( 0, y ) ( x, 0 )

        NGon r i ->
            rectFromTuples ( -r, -r ) ( 0, 2 * r ) ( 2 * r, 0 )


fromList : List Vector -> Rect
fromList transformedCorners =
    let
        xs =
            map .x transformedCorners

        ys =
            map .y transformedCorners

        minx =
            withDefault 0 (minimum xs)

        miny =
            withDefault 0 (minimum ys)

        maxx =
            withDefault 0 (maximum xs)

        maxy =
            withDefault 0 (maximum ys)

        pos =
            fromTuple ( minx, miny )

        dirA =
            fromTuple ( pos.x, maxy - miny )

        dirB =
            fromTuple ( maxx - minx, pos.y )
    in
        ( pos, dirA, dirB )


test : BodyShape -> Color -> ( Float, Float ) -> Form
test s c o =
    move o <| group [ drawShape s c, drawRect (calculate s) (dashed c) ]



--main =
--    betterCollage 1200 600
--        <| List.map3 test
--            [ Circle 50, Square 50, Rectangle 50 100, NGon 50 6, NGon 50 4 ]
--            [ Color.black, Color.blue, Color.yellow, Color.purple, Color.brown ]
--            [ ( 0, 0 ), ( 110, 0 ), ( -90, 0 ), ( 210, 0 ), ( 310, 0 ) ]

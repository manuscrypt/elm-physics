module Drawing exposing (..)

import Color exposing (..)
import Vector2 exposing (Vector, toTuple, center, origin, fromTuple, scaleBy, fromIntTuple, (<+>), (<->), normalize, perp)
import List exposing (map)
import Collage as Co exposing (..)
import Utils exposing (grid, GridRow, GridCoordinate)
import Transform exposing (translation, rotation, scaleX, scaleY, multiply)
import Model exposing (..)


solidCircle : Float -> Color -> Form
solidCircle size color =
    filled color (circle size)


withBorder : Color -> Shape -> Form
withBorder c s =
    outlined (solid c) s


drawOutlinedSquare : Float -> Color -> Form
drawOutlinedSquare size color =
    withBorder color (square size)


drawOutlinedRect : Vector -> Color -> Form
drawOutlinedRect size color =
    withBorder color (rect size.x size.y)


drawLineSegment : LineStyle -> LineSegment -> Maybe ( Float, LineStyle ) -> Form
drawLineSegment ls ( pos, dir ) maybeNormalScaleLineStyle =
    let
        p2 =
            pos <+> dir

        ( normalLen, normalLs ) =
            Maybe.withDefault ( 0, defaultLine ) maybeNormalScaleLineStyle
    in
        let
            n =
                scaleBy normalLen <| normalize <| perp dir

            c =
                center pos p2

            nLine =
                drawLine c (c <+> n) normalLs
        in
            group [ drawLine pos p2 ls, nLine ]


drawLine : Vector -> Vector -> LineStyle -> Form
drawLine p1 p2 ls =
    traced ls (path (map toTuple [ p1, p2 ]))


fillRect : Vector -> Vector -> Color -> Form
fillRect bl tr color =
    let
        size =
            tr <-> bl
    in
        filled color <| rect size.x size.y


drawRect : Rect -> LineStyle -> Form
drawRect ( pos, dirA, dirB ) ls =
    let
        tr =
            pos <+> dirA <+> dirB

        points =
            [ pos, pos <+> dirA, tr, pos <+> dirB ]

        r =
            Co.polygon (List.map toTuple points)
    in
        outlined ls r


drawOval : Oval -> LineStyle -> Form
drawOval ( pos, radii ) ls =
    move (toTuple pos) <| outlined ls (Co.oval radii.x radii.y)


drawPoly : List Vector -> LineStyle -> Form
drawPoly points ls =
    outlined ls (polygon (map toTuple points))



----- GRID


drawGrid : Vector -> Float -> Form -> Form
drawGrid size spacing form =
    let
        eff =
            Vector2.flip size

        { x, y } =
            center origin eff

        gridSize =
            ( floor (x / spacing), floor (y / spacing) )

        forms =
            List.map (drawCell spacing form) (Utils.flattenGrid (Utils.grid gridSize))

        offsets =
            List.map fromTuple [ ( 1, 1 ), ( -1, 1 ), ( -1, -1 ), ( 1, -1 ) ]

        groups =
            List.map (\{ x, y } -> groupTransform (multiply (scaleX x) (scaleY y)) forms) offsets
    in
        move (toTuple origin) <| group groups


drawCell : Float -> Form -> GridCoordinate -> Form
drawCell spacing form ( col, row ) =
    let
        pos =
            scaleBy spacing (fromIntTuple ( col, row ))
    in
        move (toTuple (origin <+> pos)) form



--move final <|
---------------------------


drawShape : BodyShape -> Color -> Form
drawShape s c =
    case s of
        Model.Circle r ->
            withBorder c (circle r)

        Model.Square r ->
            withBorder c (square r)

        Model.Rectangle x y ->
            withBorder c (rect x y)

        Model.NGon r i ->
            withBorder c (ngon i r)

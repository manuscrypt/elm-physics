module VectorAlgebra exposing (..)

import Vector2 exposing (..)
import List exposing (map, minimum, maximum)
import List.Extra exposing (uncons)
import Maybe exposing (withDefault)
import Model exposing (LineSegment, Intersection, Rect, Angle)


bl =
    Vector -1 -1


tl =
    Vector -1 1


tr =
    Vector 1 1


br =
    Vector 1 -1


ltrb =
    [ bl, tl, tr, br ]


lurd =
    [ Vector2.negate xUnit, yUnit, xUnit, Vector2.negate yUnit ]


rectFromTuples pos dirA dirB =
    ( fromTuple pos, fromTuple dirA, fromTuple dirB )


ovalFromTuples pos radii =
    ( fromTuple pos, fromTuple radii )


lineSegment : Vector -> Vector -> Float -> LineSegment
lineSegment pos dir len =
    ( pos, (scaleBy len (normalize dir)) )


pointIsOnLine : Vector -> LineSegment -> Bool
pointIsOnLine v ( p, dir ) =
    length ((v <-> p) </> dir) < length dir


pointOnSegment : Vector -> LineSegment -> Bool
pointOnSegment point ( start, dir ) =
    let
        end =
            start <+> dir

        delta =
            end <-> start

        innerProduct =
            (point.x - start.x) * delta.x + (point.y - start.y) * delta.y
    in
        0 <= innerProduct && innerProduct <= lengthSquared delta


lineLineIntersection : LineSegment -> LineSegment -> Maybe Intersection
lineLineIntersection ( pos1, dir1 ) ( pos2, dir2 ) =
    testABCD ( pos1, pos1 <+> dir1 ) ( pos2, pos2 <+> dir2 )


signed2DTriArea : Vector -> Vector -> Vector -> Float
signed2DTriArea a b c =
    (a.x - c.x) * (b.y - c.y) - (a.y - c.y) * (b.x - c.x)


testABCD : ( Vector, Vector ) -> ( Vector, Vector ) -> Maybe Intersection
testABCD ( a, b ) ( c, d ) =
    let
        a1 =
            signed2DTriArea a b d

        a2 =
            signed2DTriArea a b c
    in
        if (a1 * a2 < 0) then
            let
                a3 =
                    signed2DTriArea c d a

                a4 =
                    a3 + a2 - a1
            in
                if (a3 * a4 < 0) then
                    let
                        t =
                            a3 / (a3 - a4)

                        p =
                            a <+> scaleBy t (b <-> a)
                    in
                        Just ( t, p )
                else
                    Nothing
        else
            Nothing


rotated : Angle -> Vector -> Vector
rotated a v =
    let
        x' =
            v.x * cos (a) - v.y * sin (a)

        y' =
            v.x * sin (a) + v.y * cos (a)
    in
        fromTuple ( x', y' )


originRect : Vector -> Rect
originRect size =
    let
        pos =
            (multiply bl size)

        a =
            (multiply tl size) <-> pos

        b =
            (multiply br size) <-> pos
    in
        ( pos, a, b )


pointLineDistance : Vector -> LineSegment -> Vector
pointLineDistance p ( a, dir ) =
    let
        n =
            normalize dir

        ap =
            (a <-> p)

        projLen =
            ap <.> n

        projection =
            scaleBy projLen n
    in
        ap <-> projection


rectToSegments : Rect -> List LineSegment
rectToSegments ( p, a, b ) =
    [ ( p, a ), ( p <+> a, b ), ( p <+> a <+> b, (Vector2.negate a) ), ( p <+> b, (Vector2.negate b) ) ]


inDirectionOf : Vector -> LineSegment -> Bool
inDirectionOf dir ( lp, ld ) =
    let
        n =
            normalize (perp ld)
    in
        n <.> dir < 0

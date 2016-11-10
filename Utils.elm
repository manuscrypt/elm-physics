module Utils exposing (..)

import Element exposing (Element, sizeOf, layers)
import Collage exposing (collage, outlined, solid, rect)
import Color exposing (black)
import Random exposing (Generator, list, pair, float, generate, Seed, step)
import Vector2 exposing (Vector)
import List exposing (map)


type alias GridSize =
    ( Int, Int )


type alias GridCoordinate =
    ( Int, Int )


type alias GridRow =
    List GridCoordinate


type alias Grid =
    List GridRow


flattenGrid : Grid -> List GridCoordinate
flattenGrid grid =
    List.concatMap identity grid


grid : GridSize -> Grid
grid ( width, height ) =
    map (\i -> map ((,) i) [0..width]) [0..height]


putInBox : Element -> Element
putInBox e =
    let
        ( sx, sy ) =
            sizeOf e
    in
        layers
            [ e
            , collage sx
                sy
                [ outlined (solid Color.black)
                    (rect (toFloat sx) (toFloat sy))
                ]
            ]


vectorList : Vector -> Int -> Seed -> ( List Vector, Seed )
vectorList max howMany seed =
    let
        ( points, nextSeed ) =
            step (pointList max.x max.y howMany) seed
    in
        ( List.map Vector2.fromTuple points, nextSeed )


rndVector : Seed -> ( Vector, Seed )
rndVector s0 =
    let
        ( p1, s1 ) =
            step (float -1 1) s0

        ( p2, s2 ) =
            step (float -1 1) s1
    in
        ( Vector p1 p2, s2 )


point : Float -> Float -> Generator ( Float, Float )
point sx sy =
    pair (float -sx sx) (float -sy sy)


pointList : Float -> Float -> Int -> Generator (List ( Float, Float ))
pointList sx sy howMany =
    list howMany (point sx sy)


floatList : Float -> Float -> Int -> Generator (List Float)
floatList sx sy howMany =
    list howMany (float sx sy)


intList : Int -> Int -> Int -> Generator (List Int)
intList min max howMany =
    list howMany (Random.int min max)


probability : Generator Float
probability =
    float 0 1

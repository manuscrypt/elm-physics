module Body exposing (..)

import Vector2 exposing (Vector, (<->), (<+>), scaleBy, origin, perp)
import Model exposing (..)
import BoundingRect


gravity : Vector
gravity =
    Vector 0 -0.000981


init id shape p v theta =
    Body id shape p v (gravity) theta 1 [] []


velocityLine : Body -> LineSegment
velocityLine body =
    ( body.pos, (scaleBy 10000 body.vel) )


halfDiagonal body =
    scaleBy 0.5 (size body)


botLeft body =
    body.pos <-> halfDiagonal body


topRight body =
    body.pos <+> halfDiagonal body


topLeft body =
    body.pos <+> perp (halfDiagonal body)


botRight body =
    body.pos <-> perp (halfDiagonal body)



-------------------------------------


setPos : Body -> Position -> Body
setPos body pos =
    { body | pos = pos }


setVel : Body -> Velocity -> Body
setVel body vel =
    { body | vel = vel }


setAngle : Body -> Angle -> Body
setAngle body angle =
    { body | angle = angle }


size : Body -> Vector
size body =
    case body.shape of
        Circle r ->
            Vector r r

        Square r ->
            Vector r r

        Rectangle x y ->
            Vector x y

        NGon r i ->
            Vector r r

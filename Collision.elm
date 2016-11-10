module Collision exposing (..)

import Model exposing (..)
import VectorAlgebra exposing (..)
import Vector2 exposing (..)
import Body exposing (..)
import Physics exposing (timeToPoint)
import List exposing (map, foldl)


wallContacts : List LineSegment -> Body -> Body
wallContacts walls body =
    calcCollisions walls body



--List.foldl doCollide { body | wallProjections = [] } walls


wallProjection : Body -> LineSegment -> ( LineSegment, Vector )
wallProjection body wall =
    ( wall, pointLineDistance body.pos wall )


filterWallCollisions : Body -> ( LineSegment, Vector ) -> Bool
filterWallCollisions body ( wall, dist ) =
    pointOnSegment (body.pos <+> dist) wall && inDirectionOf body.vel wall


calcCollisions : List LineSegment -> Body -> Body
calcCollisions segments body =
    let
        relevant =
            List.filter (inDirectionOf body.vel) segments

        vectors =
            List.map (wallProjection body) relevant
    in
        { body
            | collisions = List.filterMap (collisionInfo body) relevant
            , wallProjections = List.filter (filterWallCollisions body) vectors
        }


collisionInfo : Body -> LineSegment -> Maybe ( LineSegment, Intersection )
collisionInfo body segment =
    case (lineLineIntersection (Body.velocityLine body) segment) of
        Nothing ->
            Nothing

        Just ic ->
            Just ( segment, ic )



-----------------------------------------------------------------------------


resolveWallContacts : DeltaTime -> Body -> Body
resolveWallContacts dt body =
    List.foldl (resolveWallContact dt) body body.wallProjections


resolveWallContact : DeltaTime -> ( LineSegment, Vector ) -> Body -> Body
resolveWallContact dt ( seg, dist ) body =
    let
        normal =
            normalize (perp (snd seg))

        dotNormal =
            dot dist normal
    in
        if dotNormal > 0 then
            body
        else
            let
                shouldBe =
                    .x (Body.size body)

                is =
                    Vector2.length dist

                diff =
                    shouldBe - is

                fact =
                    is / shouldBe

                collWithinTime =
                    (case collisionInfo body seg of
                        Nothing ->
                            False

                        Just ( ls, ( f, vec ) ) ->
                            (timeToPoint body.pos body.vel vec) < dt
                    )
            in
                if ((diff > 0)) then
                    -- || collWithinTime) then
                    { body | vel = wallCollision body.vel normal 1 1 }
                    --, accel = scaleBy (-fact / 10000) dist }
                else
                    body


wallCollision : Velocity -> Normal -> FrictionFactor -> RestitutionFactor -> Velocity
wallCollision vel n friction restitution =
    let
        u =
            scaleBy ((dot vel n) / (dot n n)) n
    in
        scaleBy friction (vel <-> u) <-> scaleBy restitution u



--sweep: Model->Model
--sweep model =
--  let sorted = sortBy .x model.projections
--  in foldl do model sorted
--do: Projection->Model->Model
--do p m =
--  if p.start then
--    checkCollisions p.id { m | activeBodies = m.activeBodies ++ [p.id] }
--  else
--    { m | activeBodies = filter (\n -> n /= p.id ) m.activeBodies }
--checkCollisions: BodyId->Model->Model
--checkCollisions id model =
--  if (length model.activeBodies <= 1) then
--    model
--  else
--    let active = filterMap (bodyById model) model.activeBodies
--        (first, rest) = partition (\i-> i.id == id) model.bodies
--        colls = (foldl (checkCollision  (head first)) [] rest)
--    in { model | collisions = model.collisions ++ colls }
--isColliding: Model->BodyId->Bool
--isColliding model id =
--  --List.any (\t -> t.aId == id || t.bId == id) model.collisions
--  any (\t -> t == id ) model.activeBodies
--checkCollision: Maybe Body.Model->Body.Model->List Collision->List Collision
--checkCollision tgt src oldList =
--  case tgt of
--    Nothing -> oldList
--    Just target ->
--      let (tPos, tA, tB) = BoundingRect.forBody target
--          (sPos, sA, sB) = BoundingRect.forBody src
--          d1 = distance sPos (tPos <+> tA <+> tB)
--          d2 = distance tPos (sPos <+> sA <+> sB)
--      in if (d1 <= 0 && d2 <= 0) then oldList
--      else oldList ++ [(Collision target.id src.id)]
-------------------------------------

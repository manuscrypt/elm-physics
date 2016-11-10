module Physics exposing (..)

import Vector2 exposing (..)
import Model exposing (..)

forward: DeltaTime->Body->Body
forward dt body = { body | pos = pos' body.pos body.vel body.accel dt, vel = vel' body.vel body.accel dt, collisions = [] }

----- KINEMATICS
vel': Velocity->Acceleration->Float->Velocity
vel' v0 a t = { x = v0.x + a.x * t, y = v0.y + a.y * t }

pos': Position->Velocity->Acceleration->Float->Position
pos' p0 v0 a t = { x = p0.x + v0.x * t + (0.5 * a.x * t * t), y = p0.y + v0.y * t + (0.5 * a.y * t * t) }

timeToPoint: Vector->Velocity->Vector->Time
timeToPoint from vel to =  distance from to / (Vector2.length vel)

--------------------------------

--toProjection: Body.Model->List Projection
--toProjection body = 
--  let (pos,a,b) = BoundingRect.forBody body
--  in [ { id = body.id, x = .x pos, start = True }
--     , { id = body.id, x = .x (pos <+> a), start = False } 
--     ]


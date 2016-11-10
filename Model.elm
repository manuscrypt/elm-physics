module Model exposing (..)

import Vector2 exposing (Vector)
import Random exposing (Seed)


noFx : a -> ( a, Cmd b )
noFx m =
    ( m, Cmd.none )


type alias Time =
    Float


type alias DeltaTime =
    Time


type alias Position =
    Vector


type alias Size =
    Vector


type alias Velocity =
    Vector


type alias Acceleration =
    Vector


type alias Angle =
    Float


type alias Mass =
    Float


type alias FrictionFactor =
    Float


type alias RestitutionFactor =
    Float


type alias Normal =
    Vector


type alias LineSegment =
    ( Vector, Vector )



-- pos, dir (scaled to len)


type alias Intersection =
    ( Float, Vector )



-- scale, intersection point


type alias Rect =
    ( Vector, Vector, Vector )



-- pos, dirA, dirB


type alias Oval =
    ( Vector, Vector )



-- pos, radii


type alias Polygon =
    List Vector


type alias BoolSetting =
    { name : String, state : Bool, key : Char }


type alias Settings =
    { settings : List BoolSetting }


type alias BodyId =
    Int


type BodyShape
    = Circle Float
    | Square Float
    | Rectangle Float Float
    | NGon Float Int


type alias Collision =
    { aId : BodyId, bId : BodyId }


type alias Projection =
    { id : BodyId, x : Float, start : Bool }


type alias ScreenPoint =
    ( Int, Int )


type alias MousePos =
    ScreenPoint


type alias Keys =
    { x : Int, y : Int }


type alias CharCode =
    Int


type alias Body =
    { id : BodyId
    , shape : BodyShape
    , pos : Position
    , vel : Velocity
    , accel : Acceleration
    , angle : Angle
    , mass : Mass
    , collisions : List ( LineSegment, Intersection )
    , wallProjections : List ( LineSegment, Vector )
    }


type alias Scene =
    { seed :
        Seed
        -- time
    , speedMult : Float
    , startTime : Float
    , lastDeltaT : Float
    , totalTime : Float
    , generation :
        Int
        -- inputs
    , mousePos : Vector
    , windowSize :
        Size
        -- objects
    , bodies : List Body
    , walls :
        List LineSegment
        -- calculations
    , projections : List Projection
    , activeBodies : List BodyId
    , collisions :
        List Collision
        -- settings
    , settings : Settings
    , nextId : BodyId
    }

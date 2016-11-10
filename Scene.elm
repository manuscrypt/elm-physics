module Scene exposing (..)

import Random exposing (Seed, generate)
import Vector2 exposing (Vector, negate, scaleBy, (<+>))
import List exposing (length, map, sortBy, filter, map2, any, repeat, head, concatMap, partition, foldl, filterMap)
import Utils exposing (floatList)
import Body exposing (..)
import Model exposing (..)
import Settings exposing (..)
import SceneObjectFactory exposing (..)
import VectorAlgebra exposing (..)
import Physics exposing (forward)
import Char exposing (..)
import Mouse exposing (..)


--, checkCollision, setAngle, toProjection)

import Collision exposing (..)
import KeyboardControl exposing (handleKeyPress, handleArrowKeys)


type Msg
    = Tick Time
    | KeyMsg Keys
    | Pressed Int
    | AddRandomBody Mouse.Position
      --| MouseMove mousePos
    | WindowSize { height : Int, width : Int }



--    | ArrowKeys Keys
--    | KeyPressed CharCode
---------INIT --------------


initialRandomWallCount : number
initialRandomWallCount =
    2


initWalls : List ( Vector, Vector )
initWalls =
    [ ( (Vector 400 -20), (Vector -600 -100) )
    ]


initBodies : List Body
initBodies =
    [ Body.init 0 (Model.Circle 40) (Vector 82 (150)) (Vector -0.3 0.1) 0
    , Body.init 0 (Model.Circle 20) (Vector -82 100) (Vector -0.3 0.0) 0
    ]


init : Seed -> Size -> ( Scene, Cmd Msg )
init seed size =
    let
        ( walls, seed0 ) =
            randomWalls initialRandomWallCount (scaleBy 0.2 size) seed

        frameWalls =
            rectToSegments <| originRect (scaleBy 0.45 size)
    in
        noFx
            { seed = seed0
            , startTime = 0
            , speedMult = 0.001
            , totalTime = 0
            , lastDeltaT = 0
            , generation = 0
            , mousePos = Vector 0 0
            , windowSize = size
            , bodies = initBodies
            , walls = (frameWalls ++ walls ++ initWalls)
            , projections = []
            , collisions = []
            , activeBodies = []
            , settings = Settings.init
            , nextId = 0
            }


bodyById : Scene -> BodyId -> Maybe Body
bodyById model id =
    head (filter (\t -> t.id == id) model.bodies)



-------------- UPDATE


increment : Scene -> DeltaTime -> Scene
increment model dt =
    { model
        | generation = model.generation + 1
        , lastDeltaT = dt
        , totalTime = model.totalTime + dt
        , projections = []
        , collisions = []
        , activeBodies = []
    }


contact : Scene -> Scene
contact m =
    { m | bodies = map (wallContacts m.walls) m.bodies }


stopStandingBodies : Body -> Body
stopStandingBodies b =
    if ((Vector2.length b.vel) < 0.01) then
        { b | vel = Vector2.origin }
    else
        b


resolve : Scene -> Scene
resolve m =
    { m | bodies = map (resolveWallContacts m.lastDeltaT) m.bodies }


step : Scene -> Scene
step scene =
    case Settings.getSettingStateByName "time" scene.settings of
        False ->
            scene

        True ->
            { scene | bodies = (map (Physics.forward (scene.lastDeltaT * scene.speedMult)) scene.bodies) }


update : Msg -> Scene -> ( Scene, Cmd Msg )
update action model =
    case action of
        WindowSize size ->
            noFx { model | windowSize = { x = toFloat size.width, y = toFloat size.height } }

        Tick deltaTime ->
            if model.startTime == 0 then
                noFx { model | startTime = deltaTime }
            else
                let
                    dt =
                        (deltaTime - model.startTime)
                in
                    increment model dt
                        |> contact
                        |> resolve
                        |> step
                        |> noFx

        Pressed charCode ->
            let
                char =
                    Debug.log "ch"
                        (Char.fromCode <| Debug.log "code" charCode)
            in
                noFx (KeyboardControl.handleKeyPress char model)

        KeyMsg keys ->
            Debug.log "xx" noFx (KeyboardControl.handleArrowKeys keys model)

        AddRandomBody pos ->
            let
                screenPos =
                    toScreenPos pos model

                ( newBody, seed ) =
                    randomBody model.nextId { x = toFloat pos.x, y = toFloat pos.y } model.seed
            in
                noFx ({ model | seed = seed, bodies = model.bodies ++ [ newBody ], nextId = model.nextId + 1 })


toScreenPos : Mouse.Position -> Scene -> Vector
toScreenPos { x, y } m0 =
    Vector ((toFloat x) - m0.windowSize.x / 2) (m0.windowSize.y / 2 - (toFloat y))


setRandomOrientation : Scene -> Scene
setRandomOrientation model =
    let
        ( floats, seed ) =
            Random.step (Utils.floatList 0 3.141 (length model.bodies)) model.seed
    in
        { model | bodies = (map2 Body.setAngle model.bodies floats), seed = seed }



--mf = { m0 | projections = concatMap toProjection m0.bodies }
--mf = sweep m1

module SceneView exposing (..)

import Element as El exposing (Element, show, flow, down)
import Collage as Co exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


--import Transform exposing (rotation, translation)

import Color exposing (..)
import List exposing (length, map, map2, filterMap, filter)
import List.Extra exposing (zip, lift2)
import String
import Text
import Utils exposing (putInBox, Grid, GridRow, GridCoordinate)
import Vector2 exposing (..)
import Drawing exposing (..)
import Physics exposing (..)
import VectorAlgebra exposing (..)
import KellyColors exposing (..)
import Scene exposing (..)
import Settings exposing (..)
import BoundingRect exposing (calculate)
import BoundingOval exposing (calculate)
import Elementary exposing (betterCollage, emptyForm)
import Model exposing (..)
import Body exposing (velocityLine)


---------------- VIEW SETTINGS


gridSpacing : number
gridSpacing =
    100


gridMarker : Form
gridMarker =
    filled Color.black (square 2)


axisScale =
    0.45


axisLineStyle =
    (dashed Color.black)


lineToCollisionLineStyle =
    (solid Color.gray)


bodyCornerForm =
    solidCircle 4 Color.blue


wallNormalsLineStyle =
    Just ( toFloat (15), (Co.solid Color.green) )



-----------------GENERAL HELPERS


toSecondsString : Float -> String
toSecondsString ms =
    let
        secs =
            ms / 1000
    in
        String.left 5 (toString secs)


axis : Vector -> Form
axis size =
    let
        drawAxis v =
            drawLine (origin <-> v) (origin <+> v) axisLineStyle
    in
        group (map (drawAxis << (scaleBy axisScale) << (multiply size)) [ xUnit, yUnit ])


grid : Vector -> Form
grid size =
    drawGrid size gridSpacing gridMarker


fpsView : Float -> Form
fpsView fps =
    Co.text <| Text.fromString <| (toString fps) ++ " fps"


drawIntersectionPoint : Body -> ( LineSegment, Intersection ) -> Form
drawIntersectionPoint b ( seg, ( s, ic ) ) =
    let
        time =
            timeToPoint b.pos b.vel ic

        lineToIntersection =
            drawLine b.pos ic lineToCollisionLineStyle

        intersectionPoint =
            move (toTuple ic)
                <| group
                    [ filled Color.red (circle 5)
                    , Co.text (Text.fromString (toSecondsString time))
                    ]
    in
        group [ lineToIntersection, intersectionPoint ]


drawProjectionPair : Float -> Color -> ( Projection, Projection ) -> Form
drawProjectionPair offsetY col ( pa, pb ) =
    let
        s =
            (Vector pa.x -2)

        e =
            (Vector pb.x 2)
    in
        group [ moveY offsetY <| fillRect s e col, group [ drawLine (Vector pa.x 0) s (dashed Color.gray), drawLine (Vector pb.x 0) e (dashed Color.gray) ] ]


drawProjections : Float -> List Color -> List Projection -> Form
drawProjections offsetY cols list =
    let
        starts =
            filter .start list

        ends =
            filter (\p -> (not p.start)) list

        zipped =
            zip starts ends
    in
        group (map2 (drawProjectionPair offsetY) cols zipped)



--in group (List.map (\(p1,p2) -> drawLine (Vector p1.x y) (Vector p2.x y) (solid Color.green)) model.projections)
------------------------------------------------------------------------------------
--------SETTINGS


getSceneHelper : Scene -> BoolSetting -> Maybe Form
getSceneHelper model setting =
    let
        len =
            List.length model.projections

        ( rndColors, seed0 ) =
            randomColors model.seed (len // 2)
    in
        if setting.state then
            case setting.name of
                "proj" ->
                    Just <| drawProjections (-model.windowSize.y / 2.2) rndColors model.projections

                "fps" ->
                    Just <| move (toTuple (scaleBy 0.48 model.windowSize)) <| fpsView model.lastDeltaT

                "axis" ->
                    Just <| axis model.windowSize

                "grid" ->
                    Just <| grid model.windowSize

                _ ->
                    Nothing
        else
            Nothing


getBodyHelper : Body -> BoolSetting -> Maybe Form
getBodyHelper body setting =
    if setting.state == False then
        Nothing
    else
        case setting.name of
            "cross" ->
                Just <| move (toTuple body.pos) <| filled Color.red (circle 3)

            "id" ->
                Just <| move (toTuple body.pos) <| Co.text <| Text.height 10 <| Text.fromString <| (toString body.id)

            "bRect" ->
                Just <| drawRect (BoundingRect.forBody body) (dashed Color.green)

            "bOval" ->
                Just <| drawOval (BoundingOval.forBody body) (dashed Color.green)

            "ixs" ->
                Just <| group (map (drawIntersectionPoint body) body.collisions)

            "vel" ->
                Just <| move (toTuple body.pos) <| drawLine origin (scaleBy 100 body.vel) (solid Color.green)

            --wall distance
            "wd" ->
                let
                    draw vec =
                        let
                            col =
                                if (Vector2.length vec < .x (Body.size body)) then
                                    Color.red
                                else
                                    Color.gray
                        in
                            drawLineSegment (dashed col) ( origin, vec ) Nothing
                in
                    Just <| move (toTuple body.pos) <| group (map (draw << snd) body.wallProjections)

            "r" ->
                Just
                    <| move (toTuple body.pos)
                    <| drawLineSegment (solid Color.purple) ( origin, (Body.size body) ) (Just ( toFloat (15), (Co.solid Color.black) ))

            "ray" ->
                Just <| drawLineSegment (dashed Color.green) (Body.velocityLine body) Nothing

            _ ->
                Nothing



------------------------------------------------------------------------------------
--------VIEWS------------------------------------------------------------------


view : Scene -> Html msg
view scene =
    Html.div []
        [ El.toHtml <| asElement scene
        , drawSettings
        ]


asElement : Scene -> Element
asElement scene =
    putInBox
        <| flow down
            [ betterCollage scene.windowSize.x
                scene.windowSize.y
                [ drawScene scene
                , drawTools scene
                , drawTests scene
                ]
              --    , show scene
            ]


drawSettings =
    Html.div [ Html.Attributes.style [ ( "margin-left", "20px" ) ] ]
        (List.map (\( s, c, b ) -> Html.div [] [ Html.text (s ++ ":" ++ toString c ++ " => " ++ toString b) ]) Settings.allSettings)


bodyToForm : Scene -> Body -> Form
bodyToForm model b =
    let
        ( x, y ) =
            toTuple (Body.size b)

        visibleHelpers =
            group (filterMap (getBodyHelper b) (Settings.getSettingsByState True model.settings))

        bodyColor =
            Color.black

        bodyForm =
            drawShape b.shape bodyColor |> move (toTuple b.pos)

        -- |> rotate b.angle
    in
        group [ bodyForm, visibleHelpers ]


wallToForm : LineSegment -> Form
wallToForm segment =
    drawLineSegment (solid Color.blue) segment (wallNormalsLineStyle)



--------- CONTENT ---------------


drawScene : Scene -> Form
drawScene scene =
    let
        walls =
            group (map wallToForm scene.walls)

        bodies =
            group (map (bodyToForm scene) scene.bodies)
    in
        group [ walls, bodies ]


drawTools : Scene -> Form
drawTools model =
    let
        cursor =
            move (toTuple model.mousePos) <| outlined (dashed Color.black) (square 15)

        helpers =
            group (filterMap (getSceneHelper model) (Settings.getSettingsByState True model.settings))
    in
        group [ helpers, cursor ]


drawTests : Scene -> Form
drawTests model =
    emptyForm


drawTestsxy : Scene -> Form
drawTestsxy model =
    let
        ( vPos, vA, vB ) =
            ( (Vector 10 90), (Vector 50 -25), (Vector 80 150) )

        pPos =
            (Vector 160 90)

        dist =
            pointLineDistance pPos ( vPos, vA )

        dist2 =
            pointLineDistance pPos ( vPos, vB )

        dist3 =
            pointLineDistance pPos ( vPos <+> vB, vA )

        distN =
            Debug.log "nd" (Vector2.length dist)
    in
        group
            [ drawLine vPos (vPos <+> (scaleBy 4 vA)) (solid Color.green)
            , drawRect ( vPos, vA, vB ) (solid Color.blue)
            , drawLine origin pPos (solid Color.yellow)
            , drawLine pPos (pPos <+> dist) (solid Color.red)
            , drawLine pPos (pPos <+> dist2) (solid Color.purple)
            , drawLine pPos (pPos <+> dist3) (solid Color.gray)
            ]

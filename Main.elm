module Main exposing (..)

import Vector2 exposing (Vector, scaleBy, toFloatVector, fromTuple)
import Scene exposing (init, update)
import Random exposing (Seed)
import Time exposing (every)
import SceneView exposing (view)
import Window
import Html.App as App
import Keyboard
import Mouse


main : Program Never
main =
    App.program
        { init = Scene.init (Random.initialSeed 42) (fromTuple ( 800, 600 ))
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : a -> Sub Scene.Msg
subscriptions x =
    Sub.batch
        [ Time.every (17 * Time.millisecond) Scene.Tick
        , Window.resizes Scene.WindowSize
        , Mouse.clicks Scene.AddRandomBody
        , Keyboard.presses Scene.Pressed
          --, Scene.KeyMsg Keyboard.arrows
          --, Scene.MouseMove Mouse.moves
        ]



--tick : ( Seed, Input ) -> Scene -> ( Scene, Cmd Scene.Msg )
--tick ( seed, input ) m0 =
--    case input of
--        Input.WindowSize size ->
--            Model.noFx { m0 | windowSize = Vector 800 600 }
--        Input.TimeDelta dt ->
--            update (Scene.Tick dt) m0
--        Input.MouseMove ( x, y ) ->
--            Model.noFx { m0 | mousePos =  }
--        Input.Click ->
--            update (Scene.AddRandomBody) m0
--        Input.KeyPressed charCode ->
--            update (Scene.Pressed (Char.fromCode charCode)) m0
--        Input.ArrowKeys keys ->
--            update (Scene.KeyMsg keys) m0
--inputToWindowSize : ( Int, Int ) -> Vector
--inputToWindowSize ( x, y ) =
--    scaleBy 0.98 (toFloatVector { x = x, y = y })

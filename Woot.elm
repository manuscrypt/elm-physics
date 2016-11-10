module Woot exposing (..)

import Html exposing (Html, text)
import Html.App
import Mouse
import Keyboard



main = 
 Html.App.program { init = init
                  , view = view
                  , update = update
                  , subscriptions = subscriptions }

--------------------------------------------------------------------

--MODEL

type alias Model = String

init: (Model, Cmd Msg)
init = 
 ("", Cmd.none)


--MESSAGES

type Msg 
 = MouseMsg Mouse.Position
 | KeyMsg Keyboard.KeyCode


--VIEW

view: Model -> Html Msg
view model =
 text model
   
 

--UPDATE

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
 case msg of
   MouseMsg position -> (toString position, Cmd.none)
   KeyMsg code -> (toString code, Cmd.none)



--SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model = 
 Sub.batch 
   [ Mouse.moves MouseMsg
   , Keyboard.presses KeyMsg
   ]
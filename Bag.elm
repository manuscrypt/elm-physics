module Bag exposing (..)

import List exposing (foldl, length, map)
import Html
import String exposing (fromChar)
import Char exposing (fromCode)


type alias Id =
    Int


type alias TileDef =
    { letter : String, value : Int, count : Int }


type alias Tile =
    { id : Id, def : TileDef }


type alias Model =
    { tiles : List Tile }


initTile : TileDef -> Id -> Tile
initTile def id =
    { id = id, def = def }


umlaut : Int -> String
umlaut code =
    fromChar <| fromCode code


fillBag : TileDef -> ( Int, Model ) -> ( Int, Model )
fillBag tileDef ( startId, oldBag ) =
    let
        newTiles =
            map (initTile tileDef) [0..(tileDef.count - 1)]
    in
        ( startId + (List.length newTiles), { oldBag | tiles = oldBag.tiles ++ newTiles } )


init : Model
init =
    Model []


fromList : List TileDef -> Model
fromList tileDefs =
    snd (foldl fillBag ( 0, init ) tileDefs)


german : List TileDef
german =
    [ TileDef " " 0 2
    , TileDef "E" 1 15
    , TileDef "N" 1 9
    , TileDef "S" 1 7
    , TileDef "I" 1 6
    , TileDef "R" 1 6
    , TileDef "T" 1 6
    , TileDef "U" 1 6
    , TileDef "A" 1 5
    , TileDef "D" 1 4
    , TileDef "H" 2 4
    , TileDef "G" 2 3
    , TileDef "L" 2 3
    , TileDef "O" 2 3
    , TileDef "M" 3 4
    , TileDef "B" 3 2
    , TileDef "W" 3 1
    , TileDef "Z" 3 1
    , TileDef "C" 4 2
    , TileDef "F" 4 2
    , TileDef "K" 4 2
    , TileDef "P" 4 1
    , TileDef (umlaut 196) 6 1
    , TileDef "J" 6 1
    , TileDef (umlaut 220) 6 1
    , TileDef "V" 6 1
    , TileDef (umlaut 214) 8 1
    , TileDef "X" 8 1
    , TileDef "Q" 10 1
    , TileDef "Y" 10 1
    ]


main : Html.Html a
main =
    Html.text <| toString (fromList german)

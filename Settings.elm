module Settings exposing (..)

import Model exposing (Settings, BoolSetting)


type Msg
    = ToggleSetting Char
    | SetSetting Bool BoolSetting
    | SetAll Bool


allSettings : List ( String, Char, Bool )
allSettings =
    [ ( "fps", 'F', True )
    , ( "time", 'T', True )
    , ( "ixs", 'I', True )
    , ( "grid", 'G', False )
    , ( "axis", 'A', True )
    , ( "bRect", 'B', False )
    , ( "bOval", 'O', False )
    , ( "vel", 'V', True )
    , ( "id", '1', False )
    , ( "proj", 'P', False )
    , ( "cross", 'C', True )
    , ( "wd", 'W', True )
    , ( "r", 'R', True )
    , ( "ray", '2', False )
    ]


initSetting : ( String, Char, Bool ) -> BoolSetting
initSetting ( name, key, default ) =
    BoolSetting name default key


init : Settings
init =
    { settings = List.map initSetting allSettings }


getSettingByKey : Char -> Settings -> Maybe BoolSetting
getSettingByKey key model =
    List.head (List.filter (\set -> set.key == key) model.settings)


getSettingStateByName : String -> Settings -> Bool
getSettingStateByName name model =
    case List.head (List.filter (\set -> set.name == name) model.settings) of
        Nothing ->
            False

        Just setting ->
            setting.state


getSettingsByState : Bool -> Settings -> List BoolSetting
getSettingsByState state model =
    List.filter (\set -> set.state == state) model.settings


update : Msg -> Settings -> Settings
update action model =
    case action of
        SetSetting state setting ->
            let
                updateSetting s =
                    if (setting.name == s.name) then
                        { s | state = state }
                    else
                        s
            in
                { model | settings = List.map updateSetting model.settings }

        ToggleSetting char ->
            case (getSettingByKey char model) of
                Nothing ->
                    model

                Just setting ->
                    update (SetSetting (not setting.state) setting) model

        SetAll state ->
            List.foldl (\s old -> update (SetSetting state s) old) model model.settings

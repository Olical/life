module App exposing (..)

import Html exposing (Html, text, div, p)
import Time exposing (Time, second)


type alias Model =
    { message : String
    , time : Maybe Time
    }


init : ( Model, Cmd Msg )
init =
    ( { message = "Your Elm App is working!"
      , time = Nothing
      }
    , Cmd.none
    )


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        time =
            case model.time of
                Just time ->
                    toString time

                Nothing ->
                    "No time yet!"
    in
        div []
            [ p [] [ text model.message ]
            , p [] [ text time ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

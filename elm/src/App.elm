module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class, classList)
import Time exposing (Time, second)


cellSize =
    10


worldSize =
    50


type alias Model =
    { world : List (List Int)
    }


init : ( Model, Cmd Msg )
init =
    ( { world = List.repeat worldSize (List.repeat worldSize 0)
      }
    , Cmd.none
    )


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "world" ] (List.map renderWorldRow model.world)


renderWorldRow : List Int -> Html Msg
renderWorldRow lives =
    div [ class "world-row" ] (List.map renderLife lives)


renderLife : Int -> Html Msg
renderLife life =
    div
        [ classList
            [ ( "cell", True )
            , ( "cell-alive", life == 1 )
            , ( "cell-dead", life == 0 )
            ]
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

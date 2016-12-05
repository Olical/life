module App exposing (..)

import Array exposing (Array)
import Html exposing (Html, text, div)
import Html.Attributes exposing (class, classList)
import Time exposing (Time, second)


worldSize =
    50


type alias World =
    Array (Array Bool)


type alias WorldPos =
    ( Int, Int )


type alias Model =
    { world : World
    }


init : ( Model, Cmd Msg )
init =
    let
        emptyWorld =
            Array.repeat worldSize (Array.repeat worldSize False)
    in
        ( { world = addGlider emptyWorld
          }
        , Cmd.none
        )


type Msg
    = Tick Time


setCell : WorldPos -> Bool -> World -> World
setCell ( ix, iy ) state world =
    let
        x =
            ix % worldSize

        y =
            iy % worldSize

        row =
            Array.get y world
    in
        case row of
            Nothing ->
                world

            Just row ->
                Array.set y (Array.set x state row) world


getCell : WorldPos -> World -> Maybe Bool
getCell ( ix, iy ) world =
    let
        x =
            ix % worldSize

        y =
            iy % worldSize

        row =
            Array.get x world
    in
        case row of
            Nothing ->
                Nothing

            Just row ->
                Array.get y row


addGlider : World -> World
addGlider world =
    world
        |> setCell ( 1, 0 ) True
        |> setCell ( 2, 1 ) True
        |> setCell ( 0, 2 ) True
        |> setCell ( 1, 2 ) True
        |> setCell ( 2, 2 ) True


stepWorld : World -> World
stepWorld world =
    world


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | world = stepWorld model.world }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "world" ] (Array.toList (Array.map renderWorldRow model.world))


renderWorldRow : Array Bool -> Html Msg
renderWorldRow lives =
    div [ class "world-row" ] (Array.toList (Array.map renderLife lives))


renderLife : Bool -> Html Msg
renderLife life =
    div
        [ classList
            [ ( "cell", True )
            , ( "cell-alive", life == True )
            , ( "cell-dead", life == False )
            ]
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

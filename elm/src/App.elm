module App exposing (..)

import Array exposing (Array)
import Html exposing (Html, text, div, p)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Maybe exposing (andThen, withDefault)
import Keyboard


worldSize : Int
worldSize =
    50


type alias World =
    Array (Array Bool)


type alias WorldPos =
    ( Int, Int )


type alias Model =
    { world : World
    , paused : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        emptyWorld =
            Array.repeat worldSize (Array.repeat worldSize False)
    in
        ( { world = addGlider emptyWorld
          , paused = False
          }
        , Cmd.none
        )


randomWorld : World
randomWorld =
    Array.initialize worldSize (\_ -> Array.initialize worldSize (\_ -> False))


type Msg
    = Tick Time
    | KeyPress Keyboard.KeyCode
    | ToggleLife WorldPos


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


getCell : WorldPos -> World -> Bool
getCell ( ix, iy ) world =
    let
        x =
            ix % worldSize

        y =
            iy % worldSize

        life =
            Array.get y world
                |> andThen (Array.get x)
    in
        withDefault False life


addGlider : World -> World
addGlider world =
    world
        |> setCell ( 1, 0 ) True
        |> setCell ( 2, 1 ) True
        |> setCell ( 0, 2 ) True
        |> setCell ( 1, 2 ) True
        |> setCell ( 2, 2 ) True



-- Any live cell with fewer than two live neighbours dies, as if caused by under-population.
-- Any live cell with two or three live neighbours lives on to the next generation.
-- Any live cell with more than three live neighbours dies, as if by over-population.
-- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.


shouldLive : WorldPos -> Bool -> World -> Bool
shouldLive pos life world =
    let
        lns =
            neighbours pos world
    in
        if life then
            if lns < 2 then
                False
            else if (lns == 2 || lns == 3) then
                True
            else if lns > 3 then
                False
            else
                False
        else if lns == 3 then
            True
        else
            False


stepWorld : World -> World
stepWorld world =
    Array.indexedMap
        (\y row ->
            Array.indexedMap
                (\x life ->
                    shouldLive ( x, y ) life world
                )
                row
        )
        world


neighbours : WorldPos -> World -> Int
neighbours ( x, y ) world =
    let
        offsets =
            [ ( -1, -1 )
            , ( 0, -1 )
            , ( 1, -1 )
            , ( -1, 0 )
            , ( 1, 0 )
            , ( -1, 1 )
            , ( 0, 1 )
            , ( 1, 1 )
            ]
    in
        List.foldl
            (\( ox, oy ) acc ->
                if getCell ( x + ox, y + oy ) world then
                    acc + 1
                else
                    acc
            )
            0
            offsets


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.paused then
                ( model, Cmd.none )
            else
                ( { model | world = stepWorld model.world }, Cmd.none )

        KeyPress key ->
            case key of
                32 ->
                    ( { model | paused = not model.paused }, Cmd.none )

                114 ->
                    ( { model | world = randomWorld }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleLife pos ->
            ( { model
                | world =
                    setCell pos (not (getCell pos model.world)) model.world
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "world" ]
            (Array.toList (Array.indexedMap renderWorldRow model.world))
        , pausedLabel model.paused
        ]


pausedLabel : Bool -> Html Msg
pausedLabel paused =
    p
        [ classList
            [ ( "paused", paused )
            , ( "playing", not paused )
            ]
        ]
        [ text "paused" ]


renderWorldRow : Int -> Array Bool -> Html Msg
renderWorldRow y lives =
    div [ class "world-row" ]
        (Array.toList (Array.indexedMap (\x life -> renderLife ( x, y ) life) lives))


renderLife : WorldPos -> Bool -> Html Msg
renderLife pos life =
    div
        [ classList
            [ ( "cell", True )
            , ( "cell-alive", life == True )
            , ( "cell-dead", life == False )
            ]
        , onClick (ToggleLife pos)
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (second / 10) Tick
        , Keyboard.presses KeyPress
        ]

module Vectors.MotionVelocity exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html, text)
import Math.Vector2 as V
import Processing exposing (toPoint)
import Random


type Msg
    = Frame Float
    | InitMover Mover


type alias Model =
    { mover : Maybe Mover
    }


type alias Mover =
    { location : V.Vec2
    , velocity : V.Vec2
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


width : Float
width =
    640


height : Float
height =
    360


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( { mover = Nothing }, Random.generate InitMover initMover )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( step model, Cmd.none )

        InitMover mover ->
            ( { mover = Just mover }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.mover of
        Nothing ->
            text ""

        Just mover ->
            Canvas.toHtml
                ( round width, round height )
                []
                [ clearScreen
                , shapes
                    [ fill Color.gray, stroke Color.black ]
                    [ circle (toPoint mover.location) 16 ]
                ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


step : Model -> Model
step model =
    case model.mover of
        Nothing ->
            model

        Just mover ->
            { mover = Just (updateMover mover) }


updateMover : Mover -> Mover
updateMover mover =
    let
        newLocation =
            V.toRecord <| V.add mover.location mover.velocity

        wrap min max value =
            if value < min then
                max

            else if value > max then
                min

            else
                value
    in
    { mover | location = V.vec2 (wrap 0 width newLocation.x) (wrap 0 height newLocation.y) }


initMover : Random.Generator Mover
initMover =
    Random.map4
        (\px py vx vy -> { location = V.vec2 px py, velocity = V.vec2 vx vy })
        (Random.float 0 width)
        (Random.float 0 height)
        (Random.float -2 2)
        (Random.float -2 2)

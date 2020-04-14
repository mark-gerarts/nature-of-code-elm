module Vectors.MotionRandomAcceleration exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Math.Vector2 as V
import Processing exposing (limit, toPoint)
import Random


type Msg
    = Frame Float
    | SetAcceleration V.Vec2


type alias Model =
    { mover : Mover
    }


type alias Mover =
    { location : V.Vec2
    , velocity : V.Vec2
    , acceleration : V.Vec2
    , maxSpeed : Float
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
    ( { mover =
            { location = V.vec2 (width / 2) (height / 2)
            , velocity = V.vec2 0 0
            , acceleration = V.vec2 0 0
            , maxSpeed = 10
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ mover } as model) =
    case msg of
        Frame _ ->
            ( step model, Random.generate SetAcceleration randomAcceleration )

        SetAcceleration a ->
            ( { mover = { mover | acceleration = V.scale 0.5 a } }, Cmd.none )


view : Model -> Html Msg
view { mover } =
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
step { mover } =
    { mover = updateMover mover }


updateMover : Mover -> Mover
updateMover mover =
    let
        newLocation =
            V.toRecord <| V.add mover.location mover.velocity

        newVelocity =
            limit mover.maxSpeed (V.add mover.velocity mover.acceleration)

        wrap min max value =
            if value < min then
                max

            else if value > max then
                min

            else
                value
    in
    { mover
        | location = V.vec2 (wrap 0 width newLocation.x) (wrap 0 height newLocation.y)
        , velocity = newVelocity
    }


randomAcceleration : Random.Generator V.Vec2
randomAcceleration =
    Random.map2
        (\x y -> V.normalize (V.vec2 x y))
        (Random.float -1 1)
        (Random.float -1 1)

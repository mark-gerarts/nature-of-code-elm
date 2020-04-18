module Vectors.VariableAccelerationTowardsTheMouse exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Math.Vector2 as V
import Processing exposing (limit, toPoint)


type Msg
    = Frame Float
    | MouseMoved V.Vec2


type alias Model =
    { mover : Mover
    , mousePosition : V.Vec2
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
            , maxSpeed = 5
            }
      , mousePosition = V.vec2 0 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( step model, Cmd.none )

        MouseMoved newPosition ->
            ( { model | mousePosition = newPosition }, Cmd.none )


view : Model -> Html Msg
view { mover } =
    Canvas.toHtml
        ( round width, round height )
        [ Mouse.onMove (.offsetPos >> (\( x, y ) -> V.vec2 x y) >> MouseMoved) ]
        [ clearScreen
        , shapes
            [ fill Color.gray, stroke Color.black ]
            [ circle (toPoint mover.location) 16 ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


step : Model -> Model
step ({ mover, mousePosition } as model) =
    let
        acceleration =
            V.sub mousePosition mover.location
                |> V.scale 0.1

        newMover =
            stepMover { mover | acceleration = acceleration }
    in
    { model | mover = newMover }


stepMover : Mover -> Mover
stepMover mover =
    let
        newVelocity =
            V.add mover.acceleration mover.velocity
                |> limit mover.maxSpeed

        newLocation =
            V.add newVelocity mover.location
    in
    { mover | velocity = newVelocity, location = newLocation }

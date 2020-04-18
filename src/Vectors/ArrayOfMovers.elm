module Vectors.ArrayOfMovers exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import List exposing (map)
import Math.Vector2 as V
import Processing exposing (limit, toPoint)
import Random


type Msg
    = Frame Float
    | MouseMoved V.Vec2
    | InitMovers (List Mover)


type alias Model =
    { movers : List Mover
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
    ( { movers = []
      , mousePosition = V.vec2 0 0
      }
    , Random.generate InitMovers (Random.list 10 randomMover)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( step model, Cmd.none )

        MouseMoved newPosition ->
            ( { model | mousePosition = newPosition }, Cmd.none )

        InitMovers movers ->
            ( { model | movers = movers }, Cmd.none )


view : Model -> Html Msg
view { movers } =
    Canvas.toHtml
        ( round width, round height )
        [ Mouse.onMove (.offsetPos >> (\( x, y ) -> V.vec2 x y) >> MouseMoved) ]
        [ clearScreen
        , shapes
            [ fill Color.gray, stroke Color.black ]
            (map (\mover -> circle (toPoint mover.location) 16) movers)
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


step : Model -> Model
step ({ movers, mousePosition } as model) =
    { model | movers = map (stepMover mousePosition) movers }


stepMover : V.Vec2 -> Mover -> Mover
stepMover mousePosition mover =
    let
        newAcceleration =
            V.sub mousePosition mover.location
                |> V.normalize
                |> V.scale 0.1

        newVelocity =
            V.add newAcceleration mover.velocity
                |> limit mover.maxSpeed

        newLocation =
            V.add newVelocity mover.location
    in
    { mover | velocity = newVelocity, location = newLocation, acceleration = newAcceleration }


randomMover : Random.Generator Mover
randomMover =
    Random.map2
        (\x y ->
            { location = V.vec2 x y
            , velocity = V.vec2 0 0
            , acceleration = V.vec2 0 0
            , maxSpeed = 5
            }
        )
        (Random.float 0 width)
        (Random.float 0 height)

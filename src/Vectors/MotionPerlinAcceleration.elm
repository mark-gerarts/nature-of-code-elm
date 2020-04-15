module Vectors.MotionPerlinAcceleration exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, circle, rect, shapes)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Math.Vector2 as V
import Processing exposing (limit, toPoint)
import Random
import Simplex exposing (PermutationTable, noise2d, permutationTableGenerator)


type Msg
    = Frame Float
    | CreatePermutationTable PermutationTable


type alias Model =
    { mover : Mover
    , permutationTable : Maybe PermutationTable
    , dt : Float
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
      , permutationTable = Nothing
      , dt = 0
      }
    , Random.generate CreatePermutationTable permutationTableGenerator
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( step model, Cmd.none )

        CreatePermutationTable permutationTable ->
            ( { model | permutationTable = Just permutationTable }, Cmd.none )


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
step model =
    case model.permutationTable of
        Nothing ->
            model

        Just permutationTable ->
            { model
                | mover = updateMover permutationTable model.dt model.mover
                , dt = model.dt + 0.05
            }


updateMover : PermutationTable -> Float -> Mover -> Mover
updateMover permutationTable dt mover =
    let
        newAcceleration =
            V.vec2
                (noise1d permutationTable dt)
                (noise1d permutationTable (dt + 1000))

        newLocation =
            V.toRecord <| V.add mover.location mover.velocity

        newVelocity =
            limit mover.maxSpeed (V.add mover.velocity newAcceleration)

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
        , acceleration = newAcceleration
    }


noise1d : PermutationTable -> Float -> Float
noise1d table =
    noise2d table 0

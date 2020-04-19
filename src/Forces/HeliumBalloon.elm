module Forces.HeliumBalloon exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Math.Vector2 as V
import Processing exposing (mapValue, toPoint)
import Random
import Simplex exposing (PermutationTable, noise2d, permutationTableGenerator)


type Msg
    = Frame Float
    | GeneratePermutationTable PermutationTable


type Model
    = Loading
    | Ready State


type Collision
    = Top
    | Left
    | Bottom
    | Right
    | None


type alias State =
    { mover : Mover
    , permutationTable : PermutationTable
    , dt : Float
    }


type alias Mover =
    { position : V.Vec2
    , velocity : V.Vec2
    , acceleration : V.Vec2
    , radius : Float
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
    600


height : Float
height =
    600


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


initialMover : Mover
initialMover =
    { position = V.vec2 (width / 2) (height - 20)
    , velocity = V.vec2 0 0
    , acceleration = V.vec2 0 0
    , radius = 16
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading, Random.generate GeneratePermutationTable permutationTableGenerator )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GeneratePermutationTable table, Loading ) ->
            ( Ready { mover = initialMover, permutationTable = table, dt = 0 }, Cmd.none )

        ( Frame _, Ready state ) ->
            ( Ready (step state), Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( round width, round height )
        []
        [ clearScreen
        , shapes
            [ fill Color.gray, stroke Color.black ]
            (case model of
                Loading ->
                    []

                Ready { mover } ->
                    [ circle (toPoint mover.position) mover.radius ]
            )
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


step : State -> State
step { mover, permutationTable, dt } =
    let
        newMover =
            mover
                |> handleForces permutationTable dt
                |> handleMovement
                |> handleEdges
    in
    { mover = newMover, permutationTable = permutationTable, dt = dt + 0.01 }


handleForces : PermutationTable -> Float -> Mover -> Mover
handleForces permutationTable dt mover =
    let
        gravity =
            V.vec2 0 0.1

        helium =
            V.vec2 0 -0.15

        wind =
            V.vec2
                (mapValue -1 1 -0.1 0.1 (noise2d permutationTable dt 0))
                (mapValue -1 1 -0.05 0.05 (noise2d permutationTable 0 dt))
    in
    List.foldl applyForce mover [ gravity, helium, wind ]


handleMovement : Mover -> Mover
handleMovement mover =
    { mover
        | position = V.add mover.position mover.velocity
        , velocity = V.add mover.velocity mover.acceleration
        , acceleration = V.vec2 0 0
    }


handleEdges : Mover -> Mover
handleEdges mover =
    let
        collision =
            checkCollision mover

        position =
            mover.position

        velocity =
            mover.velocity

        newPosition =
            case collision of
                Left ->
                    V.setX mover.radius position

                Right ->
                    V.setX (width - mover.radius) position

                Top ->
                    V.setY mover.radius mover.position

                Bottom ->
                    V.setY (height - mover.radius) mover.position

                None ->
                    position

        newVelocity =
            if collision == Left || collision == Right then
                V.setX (negate (V.getX velocity)) velocity
                    -- Bouncing causes us to lose some velocity.
                    |> V.scale 0.4

            else if collision == Top || collision == Bottom then
                V.setY (negate (V.getY velocity)) velocity
                    |> V.scale 0.4

            else
                velocity
    in
    { mover | position = newPosition, velocity = newVelocity }


checkCollision : Mover -> Collision
checkCollision { position, radius } =
    if V.getX position <= radius then
        Left

    else if V.getX position >= width - radius then
        Right

    else if V.getY position <= radius then
        Top

    else if V.getY position >= height - radius then
        Bottom

    else
        None


applyForce : V.Vec2 -> Mover -> Mover
applyForce f mover =
    { mover | acceleration = V.add mover.acceleration f }

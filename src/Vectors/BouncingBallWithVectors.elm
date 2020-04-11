module Vectors.BouncingBallWithVectors exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Math.Vector2 as V


type Msg
    = Frame Float


type alias Model =
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
    600


height : Float
height =
    600


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


initialModel : Model
initialModel =
    { location = V.vec2 (width / 2) (height / 2)
    , velocity = V.vec2 2.5 5
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( step model, Cmd.none )


view : Model -> Html Msg
view { location } =
    Canvas.toHtml
        ( round width, round height )
        []
        [ clearScreen
        , shapes [ fill Color.gray, stroke Color.black ] [ circle (toPoint location) 16 ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


step : Model -> Model
step { location, velocity } =
    let
        newLocation =
            V.add location velocity

        newX =
            V.getX newLocation

        newY =
            V.getY newLocation

        newVelocityX =
            if newX < 0 || newX > width then
                negate (V.getX velocity)

            else
                V.getX velocity

        newVelocityY =
            if newY < 0 || newY > height then
                negate (V.getY velocity)

            else
                V.getY velocity
    in
    { location = newLocation, velocity = V.vec2 newVelocityX newVelocityY }


toPoint : V.Vec2 -> Point
toPoint v =
    ( V.getX v, V.getY v )

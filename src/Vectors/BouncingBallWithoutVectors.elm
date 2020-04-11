module Vectors.BouncingBallWithoutVectors exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)


type Msg
    = Frame Float


type alias Model =
    { x : Float
    , y : Float
    , xSpeed : Float
    , ySpeed : Float
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
    { x = width / 2
    , y = height / 2
    , xSpeed = 1
    , ySpeed = 3.3
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
view { x, y } =
    Canvas.toHtml
        ( round width, round height )
        []
        [ clearScreen
        , shapes [ fill Color.gray, stroke Color.black ] [ circle ( x, y ) 16 ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


step : Model -> Model
step { x, y, xSpeed, ySpeed } =
    let
        newX =
            x + xSpeed

        newY =
            y + ySpeed

        newXSpeed =
            if newX < 0 || newX > width then
                negate xSpeed

            else
                xSpeed

        newYSpeed =
            if newY < 0 || newY > height then
                negate ySpeed

            else
                ySpeed
    in
    { x = newX, y = newY, xSpeed = newXSpeed, ySpeed = newYSpeed }

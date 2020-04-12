module Vectors.VectorMultiplication exposing (main)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Math.Vector2 as V
import Processing exposing (toPoint)


type Msg
    = MouseMoved ( Float, Float )


type alias Model =
    { mousePosition : V.Vec2 }


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


center : V.Vec2
center =
    V.vec2 (width / 2) (height / 2)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init () =
    ( { mousePosition = center }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        MouseMoved ( x, y ) ->
            ( { mousePosition = V.vec2 x y }, Cmd.none )


view : Model -> Html Msg
view { mousePosition } =
    Canvas.toHtml
        ( round width, round height )
        [ Mouse.onMove (.offsetPos >> MouseMoved) ]
        [ clearScreen
        , shapes
            [ stroke Color.black, transform [ translate (V.getX center) (V.getY center) ] ]
            [ path ( 0, 0 ) [ lineTo <| toPoint <| V.scale 0.5 <| V.sub mousePosition center ] ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]

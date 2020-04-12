module Vectors.TraditionalRandomWalkVectors exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Math.Vector2 as V
import Processing exposing (toPoint)
import Random


type Msg
    = Frame Float
    | IncrementPosition V.Vec2


type alias Model =
    { position : V.Vec2 }


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


init : () -> ( Model, Cmd Msg )
init () =
    ( { position = V.vec2 (width / 2) (height / 2) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( model, Random.generate IncrementPosition randomPositionIncrement )

        IncrementPosition increment ->
            ( { position = V.add increment model.position }, Cmd.none )


view : Model -> Html Msg
view { position } =
    Canvas.toHtml
        ( round width, round height )
        []
        [ shapes [ fill Color.black ] [ rect (toPoint position) 1 1 ] ]


randomPositionIncrement : Random.Generator V.Vec2
randomPositionIncrement =
    Random.map2
        (\x y -> V.vec2 (toFloat x) (toFloat y))
        (Random.int -1 1)
        (Random.int -1 1)


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )

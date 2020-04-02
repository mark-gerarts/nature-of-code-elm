module Introduction.BiasedToTheRightRandomWalk exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Random


type Direction
    = Left
    | Right
    | Up
    | Down


type Msg
    = Frame Float
    | Move Direction


type alias Model =
    { walkerPosition : Point }


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
    ( { walkerPosition = ( width / 2, height / 2 ) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( model, Random.generate Move randomDirection )

        Move direction ->
            ( { walkerPosition = move direction model.walkerPosition }, Cmd.none )


view : Model -> Html Msg
view { walkerPosition } =
    Canvas.toHtml
        ( round width, round height )
        []
        [ shapes [ fill Color.black ] [ rect walkerPosition 1 1 ] ]


{-| This is nicer than generating a point.
-}
randomDirection : Random.Generator Direction
randomDirection =
    Random.weighted ( 20, Up ) [ ( 20, Down ), ( 20, Left ), ( 40, Right ) ]


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


move : Direction -> Point -> Point
move direction =
    case direction of
        Up ->
            add ( 0, -1 )

        Down ->
            add ( 0, 1 )

        Left ->
            add ( -1, 0 )

        Right ->
            add ( 1, 0 )

module Introduction.TraditionalRandomWalk exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Random


type Msg
    = Frame Float
    | IncrementPosition ( Float, Float )


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
            ( model, Random.generate IncrementPosition randomPositionIncrement )

        IncrementPosition increment ->
            ( { walkerPosition = add increment model.walkerPosition }, Cmd.none )


view : Model -> Html Msg
view { walkerPosition } =
    Canvas.toHtml
        ( round width, round height )
        []
        [ shapes [ fill Color.black ] [ rect walkerPosition 1 1 ] ]


randomPositionIncrement : Random.Generator ( Float, Float )
randomPositionIncrement =
    Random.map2
        (\x y -> ( toFloat x, toFloat y ))
        (Random.int -1 1)
        (Random.int -1 1)


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )

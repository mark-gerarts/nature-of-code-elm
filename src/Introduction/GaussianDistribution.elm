module Introduction.GaussianDistribution exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Random
import Random.Float exposing (normal)


type Msg
    = Frame Float
    | RandomNumber Float


type alias Model =
    Maybe Float


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
    300


mean : Float
mean =
    320


sd : Float
sd =
    60


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            -- The random-extra package contains gaussian distribution
            -- generators.
            ( model, Random.generate RandomNumber (normal mean sd) )

        RandomNumber x ->
            ( Just x, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( round width, round height )
        []
        (case model of
            Nothing ->
                []

            Just x ->
                [ drawCircle x ]
        )


drawCircle : Float -> Renderable
drawCircle x =
    shapes
        [ fill (Color.rgba 0 0 0 0.1) ]
        [ circle ( x, height / 2 ) 8 ]

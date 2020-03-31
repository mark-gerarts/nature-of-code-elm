module Introduction.RandomNumberDistribution exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import List exposing (map)
import Random


type Msg
    = Frame Float
    | RandomNumber Int


type alias Model =
    Array Int


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


distributionLength : Int
distributionLength =
    20


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( Array.repeat distributionLength 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( model, Random.generate RandomNumber (Random.int 0 (distributionLength - 1)) )

        RandomNumber x ->
            ( updateCount x model, Cmd.none )


updateCount : Int -> Array Int -> Array Int
updateCount x =
    Array.indexedMap
        (\key value ->
            if key == x then
                value + 1

            else
                value
        )


view : Model -> Html Msg
view distribution =
    Canvas.toHtml
        ( round width, round height )
        []
        [ viewDistribution distribution ]


viewDistribution : Array Int -> Renderable
viewDistribution distribution =
    let
        rectWidth =
            width / toFloat (Array.length distribution)

        rectangles =
            distribution
                |> Array.indexedMap (\key count -> rect ( rectWidth * toFloat key, height - toFloat count ) rectWidth (toFloat count))
                |> Array.toList
    in
    shapes [ fill Color.gray, stroke Color.black ] rectangles

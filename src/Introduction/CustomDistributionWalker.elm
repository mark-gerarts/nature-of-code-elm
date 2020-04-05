module Introduction.CustomDistributionWalker exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Random


type Msg
    = Frame Float
    | IncrementPosition Point


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


randomPositionIncrement : Random.Generator Point
randomPositionIncrement =
    let
        randomValue =
            customDistributionValue
                |> Random.andThen (\v -> Random.float (negate v) v)
    in
    Random.map2 (\x y -> ( x, y )) randomValue randomValue


customDistributionValue : Random.Generator Float
customDistributionValue =
    Random.pair (Random.float 0 1) (Random.float 0 1)
        |> Random.andThen
            -- 2 random values, a probability p and a value v.
            (\( p, v ) ->
                if v < p ^ 2 then
                    Random.constant (v * 10)

                else
                    customDistributionValue
            )


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )

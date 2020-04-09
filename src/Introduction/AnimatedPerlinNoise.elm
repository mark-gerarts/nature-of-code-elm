module Introduction.AnimatedPerlinNoise exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Grid exposing (fold2d)
import Html exposing (Html)
import List exposing (map)
import Processing exposing (mapValue)
import Random
import Simplex exposing (PermutationTable, noise2d, permutationTableGenerator)


type Msg
    = CreatePermutationTable PermutationTable
    | Frame Float


type alias Model =
    { permutationTable : Maybe PermutationTable
    , dt : Float
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
    200


height : Float
height =
    200


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( { permutationTable = Nothing, dt = 0 }
    , Random.generate CreatePermutationTable permutationTableGenerator
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreatePermutationTable permutationTable ->
            ( { model | permutationTable = Just permutationTable }, Cmd.none )

        Frame _ ->
            ( { model | dt = model.dt + 0.05 }, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( round width, round height )
        []
        (clearScreen :: render model)


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : Model -> List Renderable
render model =
    case model.permutationTable of
        Nothing ->
            []

        Just permutationTable ->
            renderNoise permutationTable model.dt


renderNoise : PermutationTable -> Float -> List Renderable
renderNoise permutationTable dt =
    let
        coordinates =
            fold2d
                { rows = round height, cols = round width }
                (::)
                []
                |> map (\( x, y ) -> ( toFloat x, toFloat y ))

        pointToAlpha ( x, y ) =
            let
                xoffFactor =
                    0.01

                yoffFactor =
                    0.01
            in
            mapValue -1 1 0 1 (noise2d permutationTable ((x * xoffFactor) + dt) ((y * yoffFactor) + dt))

        alphaValues =
            map (\p -> ( p, pointToAlpha p )) coordinates
    in
    map (\( p, a ) -> shapes [ fill (Color.rgba 0 0 0 a) ] [ rect p 1 1 ]) alphaValues

module Introduction.TwoDimensionalPerlinNoise exposing (main)

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


type alias Model =
    { permutationTable : Maybe PermutationTable
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
    200


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init () =
    ( { permutationTable = Nothing }
    , Random.generate CreatePermutationTable permutationTableGenerator
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreatePermutationTable permutationTable ->
            ( { model | permutationTable = Just permutationTable }, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( round width, round height )
        []
        (render model)


render : Model -> List Renderable
render model =
    case model.permutationTable of
        Nothing ->
            []

        Just permutationTable ->
            renderNoise permutationTable


renderNoise : PermutationTable -> List Renderable
renderNoise permutationTable =
    let
        -- @todo: for some reason a square gets rendered instead of the full
        -- canvas
        coordinates =
            fold2d
                { rows = round width, cols = round height }
                (\( x, y ) result -> ( x, y ) :: result)
                []
                |> map (\( x, y ) -> ( toFloat x, toFloat y ))

        pointToAlpha ( x, y ) =
            let
                xoff =
                    ((y * width) + x) * 0.005

                yoff =
                    y * 0.01
            in
            mapValue -1 1 0 1 (noise2d permutationTable xoff yoff)

        alphaValues =
            map (\p -> ( p, pointToAlpha p )) coordinates
    in
    map (\( p, a ) -> shapes [ fill (Color.rgba 0 0 0 a) ] [ rect p 1 1 ]) alphaValues

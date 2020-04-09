module Introduction.ConfigurablePerlinNoise exposing (main)

{-| This is essentially a worse version of
<https://herteby.github.io/simplex-noise/preview/>, but was a nice learning
experience
-}

import Browser
import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (..)
import Color
import Debug
import Grid exposing (fold2d)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (placeholder, step, style, type_, value)
import Html.Events exposing (onInput)
import List exposing (map)
import Processing exposing (mapValue)
import Random
import Simplex exposing (..)


type Msg
    = CreatePermutationTable PermutationTable
    | ChangeFractalConfig ConfigChange


type ConfigChange
    = ChangeSteps Int
    | ChangeStepSize Float
    | ChangePersistence Float
    | ChangeScale Float


type alias Model =
    { permutationTable : Maybe PermutationTable
    , fractalConfig : FractalConfig
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
    ( { permutationTable = Nothing, fractalConfig = { steps = 6, stepSize = 2, persistence = 2, scale = 1 } }
    , Random.generate CreatePermutationTable permutationTableGenerator
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreatePermutationTable permutationTable ->
            ( { model | permutationTable = Just permutationTable }, Cmd.none )

        ChangeFractalConfig configChange ->
            ( { model | fractalConfig = updateFractalConfig configChange model.fractalConfig }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ Canvas.toHtml
            ( round width, round height )
            []
            (clearScreen :: render model)
        , div
            [ style "padding" "5px", style "background-color" "#CCC", style "margin-top" "10px" ]
            [ viewIntInput "Steps" model.fractalConfig.steps (ChangeFractalConfig << ChangeSteps) [ step "1" ]
            , viewFloatInput "Step size" model.fractalConfig.stepSize (ChangeFractalConfig << ChangeStepSize) [ step "0.1" ]
            , viewFloatInput "Scale" model.fractalConfig.scale (ChangeFractalConfig << ChangeScale) [ step "1" ]
            , viewFloatInput "Persistence" model.fractalConfig.persistence (ChangeFractalConfig << ChangePersistence) [ step "0.1" ]
            ]
        ]


render : Model -> List Renderable
render model =
    case model.permutationTable of
        Nothing ->
            []

        Just permutationTable ->
            renderNoise model.fractalConfig permutationTable


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


renderNoise : FractalConfig -> PermutationTable -> List Renderable
renderNoise fractalConfig permutationTable =
    let
        coordinates =
            fold2d
                { rows = round height, cols = round width }
                (::)
                []
                |> map (\( x, y ) -> ( toFloat x, toFloat y ))

        pointToAlpha ( x, y ) =
            let
                noiseValue =
                    fractal2d fractalConfig permutationTable x y
            in
            mapValue -1 1 0 1 noiseValue

        alphaValues =
            map (\p -> ( p, pointToAlpha p )) coordinates
    in
    map (\( p, a ) -> shapes [ fill (Color.rgba 0 0 0 a) ] [ rect p 1 1 ]) alphaValues


viewFloatInput : String -> Float -> (Float -> msg) -> List (Html.Attribute msg) -> Html msg
viewFloatInput p v toMsg otherAttributes =
    viewInput
        p
        (String.fromFloat v)
        (\value ->
            case String.toFloat value of
                Nothing ->
                    toMsg 0

                Just value_ ->
                    toMsg value_
        )
        otherAttributes


viewIntInput : String -> Int -> (Int -> msg) -> List (Html.Attribute msg) -> Html msg
viewIntInput p v toMsg otherAttributes =
    viewInput
        p
        (String.fromInt v)
        (\value ->
            case String.toInt value of
                Nothing ->
                    toMsg 0

                Just value_ ->
                    toMsg value_
        )
        otherAttributes


viewInput : String -> String -> (String -> msg) -> List (Html.Attribute msg) -> Html msg
viewInput p v toMsg otherAttributes =
    div
        []
        [ label
            [ style "margin-right" "10px", style "width" "80px", style "display" "inline-block" ]
            [ text p ]
        , input
            ([ type_ "number"
             , placeholder p
             , value v
             , onInput toMsg
             ]
                ++ otherAttributes
            )
            []
        ]


updateFractalConfig : ConfigChange -> FractalConfig -> FractalConfig
updateFractalConfig configChange config =
    case configChange of
        ChangeSteps s ->
            { config | steps = s }

        ChangePersistence p ->
            { config | persistence = p }

        ChangeScale s ->
            { config | scale = s }

        ChangeStepSize s ->
            { config | stepSize = s }

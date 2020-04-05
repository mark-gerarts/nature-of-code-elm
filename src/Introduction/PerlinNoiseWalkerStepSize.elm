module Introduction.PerlinNoiseWalkerStepSize exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Processing exposing (mapValue)
import Random
import Simplex exposing (PermutationTable, noise2d, permutationTableGenerator)


type Msg
    = Frame Float
    | CreatePermutationTable PermutationTable


type alias Model =
    { position : Point
    , tx : Float
    , ty : Float
    , permutationTable : Maybe PermutationTable
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
    600


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( { position = ( width / 2, height / 2 )
      , tx = 0
      , ty = 10000
      , permutationTable = Nothing
      }
    , Random.generate CreatePermutationTable permutationTableGenerator
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( step model, Cmd.none )

        CreatePermutationTable permutationTable ->
            ( { model | permutationTable = Just permutationTable }, Cmd.none )


view : Model -> Html Msg
view { position } =
    Canvas.toHtml
        ( round width, round height )
        []
        [ shapes [ fill Color.black ] [ rect position 1 1 ] ]


step : Model -> Model
step model =
    case model.permutationTable of
        Nothing ->
            model

        Just permutationTable ->
            let
                newPosition =
                    add model.position (randomPositionIncrement permutationTable model.tx model.ty)

                inc x =
                    x + 0.005
            in
            { model | tx = inc model.tx, ty = inc model.ty, position = newPosition }


randomPositionIncrement : PermutationTable -> Float -> Float -> Point
randomPositionIncrement permutationTable tx ty =
    let
        stepSize x =
            noise1d permutationTable x
                |> mapValue -1 1 -3 3
    in
    ( stepSize tx, stepSize ty )


noise1d : PermutationTable -> Float -> Float
noise1d table =
    noise2d table 0


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )

module Introduction.PerlinNoiseWalker exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Color
import Html exposing (Html)
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
        [ clearScreen, renderCircle position ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


renderCircle : Point -> Renderable
renderCircle position =
    shapes
        [ fill Color.gray, stroke Color.black, lineWidth 2 ]
        [ circle position 16 ]


step : Model -> Model
step model =
    case model.permutationTable of
        Nothing ->
            model

        Just permutationTable ->
            let
                newX =
                    noise1d permutationTable model.tx
                        -- Unlike Processing's `noise` function, the simplex
                        -- library generates values between -1 and 1.
                        |> mapValue -1 1 0 width

                newY =
                    noise1d permutationTable model.ty
                        |> mapValue -1 1 0 height
            in
            { model | tx = model.tx + 0.005, ty = model.ty + 0.005, position = ( newX, newY ) }


{-| The simplex library doesn't provide a noise1d, so we'll use noise2d with a
fixed x-value.
-}
noise1d : PermutationTable -> Float -> Float
noise1d table =
    noise2d table 0


{-| Recreates Processing's `map` function, which projects a bounded value to new
bounds.
-}
mapValue : Float -> Float -> Float -> Float -> Float -> Float
mapValue min max newMin newMax x =
    newMin + (newMax - newMin) * ((x - min) / (max - min))

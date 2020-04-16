module Vectors.CarSimulation exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Texture exposing (..)
import Color
import Html exposing (Html)
import Math.Vector2 as V
import Processing exposing (toPoint)


type Msg
    = Frame Float
    | TextureLoaded (Maybe Texture)


type Model
    = Loading
    | Ready
        { carPosition : V.Vec2
        , sprite : Texture
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
    300


center : V.Vec2
center =
    V.vec2 (width / 2) (height / 2)


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( model, Cmd.none )

        TextureLoaded Nothing ->
            ( model, Cmd.none )

        TextureLoaded (Just sprite) ->
            ( Ready { carPosition = center, sprite = sprite }, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtmlWith
        { width = round width
        , height = round height
        , textures = [ loadFromImageUrl "./docs/static/img/spr_vintage_1.png" TextureLoaded ]
        }
        []
        [ clearScreen, renderModel model ]


renderModel : Model -> Renderable
renderModel model =
    case model of
        Loading ->
            text [] ( 50, 50 ) "Loading"

        Ready { carPosition, sprite } ->
            texture [] (toPoint carPosition) sprite


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]

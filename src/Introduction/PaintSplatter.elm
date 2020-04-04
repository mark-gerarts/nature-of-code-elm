module Introduction.PaintSplatter exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color exposing (..)
import Html exposing (Html)
import List exposing (length, map)
import Random
import Random.Float exposing (normal)


type Msg
    = Frame Float
    | RandomSplatter Splatter
    | BaseColor Color


type alias Model =
    { splatters : List Splatter
    , baseColor : Maybe Color
    }


type alias Splatter =
    { position : Point
    , color : Color
    , radius : Float
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


maxSplatters : Int
maxSplatters =
    100


width : Float
width =
    600


height : Float
height =
    600


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Frame


init : () -> ( Model, Cmd Msg )
init () =
    ( { splatters = [], baseColor = Nothing }
    , Random.generate BaseColor randomColor
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BaseColor c ->
            ( { model | baseColor = Just c }, Cmd.none )

        Frame _ ->
            ( model
            , case model.baseColor of
                Nothing ->
                    Cmd.none

                Just baseColor ->
                    if length model.splatters < maxSplatters then
                        Random.generate RandomSplatter (randomSplatter baseColor)

                    else
                        Cmd.none
            )

        RandomSplatter splatter ->
            ( { model | splatters = splatter :: model.splatters }, Cmd.none )


view : Model -> Html Msg
view { splatters } =
    Canvas.toHtml
        ( round width, round height )
        []
        (clearScreen :: map renderSplatter splatters)


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


renderSplatter : Splatter -> Renderable
renderSplatter { position, color, radius } =
    shapes
        [ fill color ]
        [ circle position radius ]


randomSplatter : Color -> Random.Generator Splatter
randomSplatter baseColor =
    let
        -- Cluster the splatters around the center with the normal distribution.
        position =
            Random.pair
                (normal centerX 60)
                (normal centerY 60)

        -- Make the splatters opaque, but mostly solid.
        opacity =
            normal 0 0.4
                |> Random.map (\a -> 1 - abs a)

        maxDistance =
            distance ( centerX, centerY ) ( 0, 0 )

        -- The further away from the center, the smaller the splatters tend to
        -- get.
        growthFactor p =
            1 - (distance p ( centerX, centerY ) / maxDistance)

        radius =
            normal 25 5

        rgba =
            Color.toRgba baseColor
    in
    Random.map3
        (\p o r ->
            { position = p
            , color = Color.fromRgba { rgba | alpha = o }
            , radius = r * growthFactor p
            }
        )
        position
        opacity
        radius


randomColor : Random.Generator Color
randomColor =
    Random.map3 (\r g b -> Color.rgb r g b)
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


distance : Point -> Point -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

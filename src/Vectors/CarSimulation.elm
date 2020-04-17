module Vectors.CarSimulation exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Texture exposing (..)
import Color
import Html exposing (Html)
import Json.Decode as Decode
import Math.Vector2 as V
import Processing exposing (limit, toPoint)


type Msg
    = Frame Float
    | TextureLoaded (Maybe Texture)
    | KeyDown Direction
    | KeyUp Direction


type Model
    = Loading
    | Ready Car


type alias Car =
    { position : V.Vec2
    , velocity : V.Vec2
    , acceleration : V.Vec2
    , sprite : Texture
    , state : State
    }


type Direction
    = Left
    | Right
    | Other


type State
    = Accelerating Direction
    | NoInput


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
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown (Decode.map KeyDown keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
        ]


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case ( model, msg ) of
                ( Loading, TextureLoaded (Just sprite) ) ->
                    Ready
                        { position = center
                        , sprite = sprite
                        , acceleration = V.vec2 0 0
                        , velocity = V.vec2 0 0
                        , state = NoInput
                        }

                ( Ready car, Frame _ ) ->
                    Ready (stepCar car)

                ( Ready car, KeyDown direction ) ->
                    if car.state == NoInput then
                        Ready { car | state = Accelerating direction }

                    else
                        model

                ( Ready car, KeyUp direction ) ->
                    case car.state of
                        Accelerating currentDirection ->
                            if direction == currentDirection then
                                Ready { car | state = NoInput }

                            else
                                model

                        NoInput ->
                            model

                ( _, _ ) ->
                    model
    in
    ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    Canvas.toHtmlWith
        { width = round width
        , height = round height

        -- If you're viewing this sketch locally, change to ./docs/static instead.
        , textures = [ loadFromImageUrl "../../static/img/spr_vintage_1.png" TextureLoaded ]
        }
        []
        [ clearScreen
        , renderModel model
        ]


renderModel : Model -> Renderable
renderModel model =
    case model of
        Loading ->
            text [] ( 50, 50 ) "Loading"

        Ready car ->
            texture [] (toPoint car.position) car.sprite


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


stepCar : Car -> Car
stepCar =
    handleAcceleration >> handleVelocity >> handlePosition


handlePosition : Car -> Car
handlePosition car =
    let
        newPosition =
            V.add car.position car.velocity

        wrappedPosition =
            if V.getX newPosition > width then
                V.setX 0 newPosition

            else if V.getX newPosition < 0 then
                V.setX width newPosition

            else
                newPosition
    in
    { car | position = wrappedPosition }


handleVelocity : Car -> Car
handleVelocity car =
    { car | velocity = limit 10 (V.add car.velocity car.acceleration) }


handleAcceleration : Car -> Car
handleAcceleration car =
    case car.state of
        NoInput ->
            slowDown car

        Accelerating direction ->
            let
                increment =
                    0.01

                incrementVector =
                    if direction == Left then
                        V.vec2 (negate increment) 0

                    else
                        V.vec2 increment 0
            in
            { car | acceleration = V.add incrementVector car.acceleration }


slowDown : Car -> Car
slowDown car =
    if V.length car.velocity < 0.5 then
        { car | acceleration = V.vec2 0 0, velocity = V.vec2 0 0 }

    else
        { car | acceleration = V.scale -0.2 (V.normalize car.velocity) }


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        _ ->
            Other

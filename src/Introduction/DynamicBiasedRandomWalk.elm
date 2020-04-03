module Introduction.DynamicBiasedRandomWalk exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Random


type Msg
    = Frame Float
    | MoveMsg Move
    | MouseMoved Point


type Move
    = ToMouse
    | AddPoint Point


type alias Model =
    { walkerPosition : Point
    , mousePosition : Point
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
    let
        center =
            ( width / 2, height / 2 )
    in
    ( { walkerPosition = center, mousePosition = center }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( model, Random.generate MoveMsg randomMove )

        MoveMsg move ->
            case move of
                ToMouse ->
                    ( { model | walkerPosition = moveOneStepToPoint model.mousePosition model.walkerPosition }, Cmd.none )

                AddPoint p ->
                    ( { model | walkerPosition = add p model.walkerPosition }, Cmd.none )

        MouseMoved p ->
            ( { model | mousePosition = p }, Cmd.none )


view : Model -> Html Msg
view { walkerPosition } =
    Canvas.toHtml
        ( round width, round height )
        [ Mouse.onMove (.offsetPos >> MouseMoved) ]
        [ shapes [ fill Color.black ] [ rect walkerPosition 1 1 ] ]


{-| Give the walker a 50% chance of moving to the mouse position. Very useful to
learn about Elm's random library!
-}
randomMove : Random.Generator Move
randomMove =
    Random.uniform True [ False ]
        |> Random.andThen
            (\moveToMouse ->
                if moveToMouse then
                    Random.constant ToMouse

                else
                    Random.pair (Random.float -1 1) (Random.float -1 1)
                        |> Random.map (\p -> AddPoint p)
            )


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| Technically we don't know about vectors yet, but this way the walker moves
"straight" to the mouse, as opposed to using a naive implementation
-}
moveOneStepToPoint : Point -> Point -> Point
moveOneStepToPoint dest src =
    let
        diff ( x1, y1 ) ( x2, y2 ) =
            ( x1 - x2, y1 - y2 )

        magnitude ( x, y ) =
            sqrt (x ^ 2 + y ^ 2)

        div ( x1, y1 ) n =
            ( x1 / n, y1 / n )

        normalize v =
            div v (magnitude v)
    in
    add src (normalize (diff dest src))

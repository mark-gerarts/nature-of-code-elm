module Processing exposing
    ( mapValue
    , limit, toPoint
    )

{-| Provides common helper functions that are part of Processing, but don't
have a suitable Elm alternative.


# Available functions

@docs mapValue

-}

import Canvas exposing (Point)
import Math.Vector2 as V


{-| Recreates Processing's `map` function, which projects a bounded value to new
bounds.

    mapValue 0 1 0 100 0.1 == 10

-}
mapValue : Float -> Float -> Float -> Float -> Float -> Float
mapValue min max newMin newMax x =
    newMin + (newMax - newMin) * ((x - min) / (max - min))


{-| Technically not really part of P5, but it's used so often with the
combination of elm-canvas and Vec2 that we might as well put it here.
-}
toPoint : V.Vec2 -> Point
toPoint v =
    ( V.getX v, V.getY v )


{-| Limits a vector to the given size by normalizing and then scaling.

    limit 10 (vec2 20 0) == vec2 10 0

-}
limit : Float -> V.Vec2 -> V.Vec2
limit max v =
    if V.length v <= max then
        v

    else
        V.scale max (V.normalize v)

module Processing exposing (mapValue)

{-| Provides common helper functions that are part of Processing, but don't
have a suitable Elm alternative.


# Available functions

@docs mapValue

-}


{-| Recreates Processing's `map` function, which projects a bounded value to new
bounds.

    mapValue 0 1 0 100 0.1 == 10

-}
mapValue : Float -> Float -> Float -> Float -> Float -> Float
mapValue min max newMin newMax x =
    newMin + (newMax - newMin) * ((x - min) / (max - min))

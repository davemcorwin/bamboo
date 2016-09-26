module StyleHelper exposing (..)

import Style exposing (Style)


type alias Styles =
    List Style


gridGap : String -> Style
gridGap b =
    ( "grid-gap", b )


gridTemplateColumns : String -> Style
gridTemplateColumns b =
    ( "grid-template-columns", b )


gridTemplateRows : String -> Style
gridTemplateRows b =
    ( "grid-template-rows", b )


gridRow : Int -> Int -> Style
gridRow start end =
    ( "grid-row", (toString start) ++ " / " ++ (toString end) )


gridColumn : Int -> Int -> Style
gridColumn start end =
    ( "grid-column", (toString start) ++ " / " ++ (toString end) )


grid : String
grid =
    "grid"


pointerEvents : String -> Style
pointerEvents b =
    ( "pointer-events", b )

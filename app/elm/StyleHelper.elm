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


gridRow : String -> Style
gridRow b =
    ( "grid-row", b )


gridColumn : String -> Style
gridColumn b =
    ( "grid-column", b )


grid : String
grid =
    "grid"

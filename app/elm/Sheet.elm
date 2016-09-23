module Sheet exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, style)
import Style exposing (..)
import StyleHelper exposing (..)
import String


-- Model


type alias Model =
    { numCols : Int
    , numRows : Int
    , dfltColWidth : Int
    , dfltRowHeight : Int
    , colHeaderColWidth : Int
    }


init : Model
init =
    { numCols = 20
    , numRows = 20
    , dfltColWidth = 100
    , dfltRowHeight = 35
    , colHeaderColWidth = 50
    }



-- Util


alpha : Int -> String
alpha idx =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> String.slice idx (idx + 1)


rowsFun : Model -> String
rowsFun model =
    model.dfltRowHeight
        |> List.repeat model.numRows
        |> List.map px
        |> String.join " "


colsFun : Model -> String
colsFun model =
    model.dfltColWidth
        |> List.repeat model.numCols
        |> List.map px
        |> String.join " "


gridLayoutStyle : String -> String -> Styles
gridLayoutStyle rows cols =
    [ display grid
    , gridGap (px 1)
    , gridTemplateColumns cols
    , gridTemplateRows rows
    ]


gridLayout : (Model -> String) -> (Model -> String) -> (Model -> List (Html.Html msg)) -> Model -> Html.Html msg
gridLayout rows cols children model =
    div
        [ style (gridLayoutStyle (rows model) (cols model)) ]
        (children model)


gridCellStyle : String -> String -> Styles
gridCellStyle row col =
    [ gridRow row
    , gridColumn col
    ]


dataCellStyle : String -> String -> Styles
dataCellStyle row col =
    List.concat
        [ gridCellStyle row col
        , [ padding (px 8)
          , position relative
          , cursor "cell"
          ]
        ]


dataCell : String -> String -> String -> Html.Html msg
dataCell row col value =
    div
        [ class "data-cell", style (dataCellStyle row col) ]
        [ text value ]


headerCell : String -> String -> String -> Html.Html msg
headerCell row col value =
    div
        [ class "header-cell", style (gridCellStyle row col) ]
        [ text value ]



-- Corner Cell


cornerCellStyle : Model -> Styles
cornerCellStyle model =
    [ width (px (model.colHeaderColWidth + 1))
    , height (px (model.dfltRowHeight + 1))
    ]


cornerCell : Model -> Html.Html msg
cornerCell model =
    div [ class "corner-cell", style (cornerCellStyle model) ] []



-- Row Header


rowHeaderCells : Model -> List (Html.Html msg)
rowHeaderCells model =
    [1..model.numCols]
        |> List.map (\v -> headerCell "1" (toString v) (alpha (v - 1)))


rowHeaderStyle : Model -> Html.Attribute msg
rowHeaderStyle model =
    style
        [ ( "width", px ((model.dfltColWidth + 1) * model.numCols + model.colHeaderColWidth + 1) )
        , ( "height", px (model.dfltRowHeight + 1) )
        ]


rowHeader : Model -> Html.Html msg
rowHeader model =
    div [ class "sticky row-header", rowHeaderStyle model ]
        [ gridLayout (\m -> px m.dfltRowHeight) colsFun rowHeaderCells model ]



-- Col Header


colHeaderCells : Model -> List (Html.Html msg)
colHeaderCells model =
    [1..model.numRows]
        |> List.map (\v -> headerCell (toString v) "1" (toString v))


colHeaderStyle : Model -> Styles
colHeaderStyle model =
    [ height (px ((model.dfltRowHeight) * (model.numRows + 1)))
    , width (px (model.colHeaderColWidth + 1))
    ]


colHeader : Model -> Html.Html msg
colHeader model =
    div [ class "sticky col-header", style (colHeaderStyle model) ]
        [ gridLayout rowsFun (\m -> px (m.dfltColWidth // 2)) colHeaderCells model ]



-- Data


dataCells : Model -> List (Html.Html msg)
dataCells model =
    List.concatMap (\r -> List.map (\c -> dataCell (toString r) (toString c) "") [1..model.numCols]) [1..model.numRows]


dataStyle : Model -> Styles
dataStyle model =
    [ position absolute
    , left (px ((model.dfltColWidth // 2) + 1))
    , top (px (model.dfltRowHeight + 1))
    ]


data : Model -> Html.Html msg
data model =
    div [ style (dataStyle model) ]
        [ gridLayout rowsFun colsFun dataCells model ]



-- Sheet


sheetStyle : Model -> Styles
sheetStyle model =
    [ position relative
    , width (px ((model.dfltColWidth + 1) * model.numCols + model.colHeaderColWidth + 1))
    , height (px ((model.dfltRowHeight + 1) * (model.numRows + 1)))
    ]


sheet : Html.Html msg
sheet =
    div [ id "sheet", style (sheetStyle init) ]
        [ data init
        , cornerCell init
        , rowHeader init
        , colHeader init
        ]

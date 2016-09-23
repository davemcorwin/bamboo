module Sheet exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (id, class, style)
import Style exposing (..)
import StyleHelper exposing (..)
import String


-- Model


type alias Selection =
    { startRow : Int
    , endRow : Int
    , startColumn : Int
    , endColumn : Int
    }


type alias Model =
    { numCols : Int
    , numRows : Int
    , dfltColWidth : Int
    , dfltRowHeight : Int
    , colHeaderColWidth : Int
    , selection : Selection
    }


init : Model
init =
    { numCols = 20
    , numRows = 20
    , dfltColWidth = 100
    , dfltRowHeight = 35
    , colHeaderColWidth = 50
    , selection =
        { startRow = 2
        , endRow = 4
        , startColumn = 3
        , endColumn = 6
        }
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


gridLayout : (Model -> String) -> (Model -> String) -> (Model -> List (Html msg)) -> Model -> Html msg
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


dataCell : String -> String -> String -> Html msg
dataCell row col value =
    div
        [ class "data-cell", style (dataCellStyle row col) ]
        [ text value ]


headerCell : String -> String -> String -> Html msg
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


cornerCell : Model -> Html msg
cornerCell model =
    div [ class "corner-cell", style (cornerCellStyle model) ] []



-- Row Header


rowHeaderCells : Model -> List (Html msg)
rowHeaderCells model =
    [1..model.numCols]
        |> List.map (\v -> headerCell "1" (toString v) (alpha (v - 1)))


rowHeaderStyle : Model -> Styles
rowHeaderStyle model =
    [ width (px ((model.dfltColWidth + 1) * model.numCols + model.colHeaderColWidth + 1))
    , height (px (model.dfltRowHeight + 1))
    ]


rowHeader : Model -> Html msg
rowHeader model =
    div [ class "sticky row-header", style (rowHeaderStyle model) ]
        [ gridLayout (\m -> px m.dfltRowHeight) colsFun rowHeaderCells model ]



-- Col Header


colHeaderCells : Model -> List (Html msg)
colHeaderCells model =
    [1..model.numRows]
        |> List.map (\v -> headerCell (toString v) "1" (toString v))


colHeaderStyle : Model -> Styles
colHeaderStyle model =
    [ height (px ((model.dfltRowHeight) * (model.numRows + 1)))
    , width (px (model.colHeaderColWidth + 1))
    ]


colHeader : Model -> Html msg
colHeader model =
    div [ class "sticky col-header", style (colHeaderStyle model) ]
        [ gridLayout rowsFun (\m -> px (m.dfltColWidth // 2)) colHeaderCells model ]



-- Selection


selectionStyle : Model -> Styles
selectionStyle model =
    [ border "1px solid rgb(237,134,100)"
    , backgroundColor "rgba(237,134,100, .1)"
    , cursor "cell"
    , gridRow ((toString model.selection.startRow) ++ " / " ++ (toString (model.selection.endRow + 2)))
    , gridColumn ((toString model.selection.startColumn) ++ " / " ++ (toString (model.selection.endColumn + 2)))
    , zIndex "5"
    , pointerEvents none
    ]


selection : Model -> List (Html msg)
selection model =
    [ div [ style (selectionStyle model) ] [] ]



-- Active


activeStyle : Model -> Styles
activeStyle model =
    [ gridRow (toString model.selection.startRow)
    , gridColumn (toString model.selection.startColumn)
    , zIndex "7"
    , cursor "text"
    , backgroundColor "rgb(48,53,65)"
    , borderTopWidth (px 1)
    , borderLeftWidth (px 1)
    , borderTopStyle solid
    , borderLeftStyle solid
    , borderTopColor "rgb(237,134,100)"
    , borderLeftColor "rgb(237,134,100)"
    ]


active : Model -> List (Html msg)
active model =
    [ div [ class "active-cell", style (activeStyle model) ] [] ]



-- Data


dataCells : Model -> List (Html msg)
dataCells model =
    List.concatMap (\r -> List.map (\c -> dataCell (toString r) (toString c) "") [1..model.numCols]) [1..model.numRows]


dataStyle : Model -> Styles
dataStyle model =
    [ position absolute
    , left (px ((model.dfltColWidth // 2) + 1))
    , top (px (model.dfltRowHeight + 1))
    ]


data : Model -> Html msg
data model =
    div [ style (dataStyle model) ]
        [ gridLayout rowsFun colsFun (\m -> List.concat [ dataCells m, selection m, active m ]) model ]



-- Sheet


sheetStyle : Model -> Styles
sheetStyle model =
    [ position relative
    , width (px ((model.dfltColWidth + 1) * model.numCols + model.colHeaderColWidth + 1))
    , height (px ((model.dfltRowHeight + 1) * (model.numRows + 1)))
    ]


sheet : Html msg
sheet =
    div [ id "sheet", style (sheetStyle init) ]
        [ data init
        , cornerCell init
        , rowHeader init
        , colHeader init
        ]

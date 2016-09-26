module Main exposing (main)

import Html exposing (Html, div, text)
import Html.App as App
import Html.Attributes exposing (id, class, style)
import Html.Events exposing (onClick)
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
        { startRow = 1
        , endRow = 1
        , startColumn = 1
        , endColumn = 1
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


gridLayout : (Model -> String) -> (Model -> String) -> List (Html Msg) -> Model -> Html Msg
gridLayout rows cols children model =
    div
        [ class "grid"
        , style
            [ gridTemplateColumns (cols model)
            , gridTemplateRows (rows model)
            ]
        ]
        children



-- Data Cell


dataCell : Int -> Int -> String -> Html Msg
dataCell row col value =
    div
        [ class "data-cell"
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        , onClick (SelectCell row col)
        ]
        [ text value ]



-- Header Cell


headerCell : Int -> Int -> String -> Html Msg
headerCell row col value =
    div
        [ class "header-cell"
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        ]
        [ text value ]



-- Corner Cell


cornerCell : Model -> Html Msg
cornerCell model =
    div
        [ class "corner-cell"
        , style
            [ width (px (model.colHeaderColWidth + 1))
            , height (px (model.dfltRowHeight + 1))
            ]
        ]
        []



-- Selection Cell


selectionCell : Int -> Int -> Bool -> Html Msg
selectionCell row col idx =
    div
        [ class
            ("selection-cell"
                ++ case idx of
                    True ->
                        " active"

                    _ ->
                        ""
            )
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        ]
        []



-- Row Header


rowHeader : Model -> Html Msg
rowHeader model =
    let
        cells =
            [1..model.numCols]
                |> List.map (\v -> headerCell 1 v (alpha (v - 1)))
    in
        div
            [ class "row-header"
            , style
                [ width (px ((model.dfltColWidth + 1) * model.numCols + model.colHeaderColWidth + 1))
                , height (px (model.dfltRowHeight + 1))
                , marginLeft (px (model.dfltColWidth // 2 + 1))
                ]
            ]
            [ gridLayout (\m -> px m.dfltRowHeight) colsFun cells model ]



-- Col Header


colHeader : Model -> Html Msg
colHeader model =
    let
        cells =
            [1..model.numRows]
                |> List.map (\v -> headerCell v 1 (toString v))
    in
        div
            [ class "col-header"
            , style
                [ height (px ((model.dfltRowHeight) * (model.numRows + 1)))
                , width (px (model.colHeaderColWidth + 1))
                ]
            ]
            [ gridLayout rowsFun (\m -> px (m.dfltColWidth // 2)) cells model ]



-- Data


data : Model -> Html Msg
data model =
    let
        cells =
            List.concatMap
                (\row ->
                    List.map
                        (\col -> dataCell row col "")
                        [1..model.numCols]
                )
                [1..model.numRows]

        selectionCells =
            [model.selection.startRow..model.selection.endRow]
                |> List.indexedMap
                    (\rowIdx row ->
                        List.indexedMap
                            (\colIdx col -> selectionCell row col (rowIdx + colIdx == 0))
                            [model.selection.startColumn..model.selection.endColumn]
                    )
                |> List.concat

        selectionRange =
            [ div
                [ class "selection-range"
                , style
                    [ gridRow model.selection.startRow (model.selection.endRow + 1)
                    , gridColumn model.selection.startColumn (model.selection.endColumn + 1)
                    ]
                ]
                []
            ]
    in
        div
            [ class "data"
            , style
                [ left (px ((model.dfltColWidth // 2) + 1))
                , top (px (model.dfltRowHeight + 1))
                ]
            ]
            [ gridLayout rowsFun colsFun (List.concat [ cells, selectionCells, selectionRange ]) model ]



-- Sheet


sheet : Model -> Html Msg
sheet model =
    div
        [ id "sheet"
        , style
            [ width (px ((model.dfltColWidth + 1) * model.numCols + model.colHeaderColWidth + 1))
            , height (px ((model.dfltRowHeight + 1) * (model.numRows + 1)))
            ]
        ]
        [ data model
        , cornerCell model
        , rowHeader model
        , colHeader model
        ]



-- View


view : Model -> Html Msg
view model =
    sheet model



-- Update


type Msg
    = NoOp
    | SelectCell Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectCell row col ->
            let
                foo =
                    model.selection

                selection =
                    { foo | startRow = row, endRow = row, startColumn = col, endColumn = col }
            in
                { model | selection = selection }

        NoOp ->
            model



-- App


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , view = sheet
        , update = update
        }

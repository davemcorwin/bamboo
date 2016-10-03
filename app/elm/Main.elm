port module Main exposing (main)

import Html exposing (Attribute, Html, div, text)
import Html.App as App
import Html.Attributes exposing (id, class, style)
import Html.Events exposing (keyCode, onClick, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import Style exposing (..)
import StyleHelper exposing (..)
import String
import Keyboard exposing (KeyCode)


-- Model


type alias Selection =
    { startRow : Int
    , endRow : Int
    , startColumn : Int
    , endColumn : Int
    }


type alias ActiveCell =
    { row : Int
    , column : Int
    }


type alias Model =
    { numCols : Int
    , numRows : Int
    , dfltColWidth : Int
    , dfltRowHeight : Int
    , colHeaderColWidth : Int
    , dragging : Bool
    , activeCell : ActiveCell
    , selection : Selection
    }


init : ( Model, Cmd Msg )
init =
    ( { numCols = 20
      , numRows = 20
      , dfltColWidth = 100
      , dfltRowHeight = 35
      , colHeaderColWidth = 50
      , dragging = False
      , activeCell =
            { row = 1
            , column = 1
            }
      , selection =
            { startRow = 1
            , endRow = 1
            , startColumn = 1
            , endColumn = 1
            }
      }
    , Cmd.none
    )



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
        , onMouseDown (DragStart row col)
        , onMouseUp (DragEnd row col)
        , onMouseEnter (DragMove row col)
        ]
        [ text value ]



-- Header Cell


headerCell : Int -> Int -> String -> Msg -> Html Msg
headerCell row col value msg =
    div
        [ class "header-cell"
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        , onClick msg
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
        , onClick SelectAll
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
                |> List.map (\col -> headerCell 1 col (alpha (col - 1)) (SelectColumn col))
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
                |> List.map (\row -> headerCell row 1 (toString row) (SelectRow row))
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
                            (\colIdx col -> selectionCell row col (row == model.activeCell.row && col == model.activeCell.column))
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
    | DragEnd Int Int
    | DragMove Int Int
    | DragStart Int Int
    | KeyDown KeyCode
    | SelectAll
    | SelectColumn Int
    | SelectRow Int


updateHelper : Model -> ( Model, Cmd Msg )
updateHelper model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ activeCell, selection } as model) =
    let
        result =
            case msg of
                KeyDown keyCode ->
                    case keyCode of
                        -- left
                        37 ->
                            let
                                col =
                                    max 1 (activeCell.column - 1)
                            in
                                { model
                                    | activeCell = { activeCell | column = col }
                                    , selection = Selection activeCell.row activeCell.row col col
                                }

                        -- up
                        38 ->
                            let
                                row =
                                    max 1 (activeCell.row - 1)
                            in
                                { model
                                    | activeCell = { activeCell | row = row }
                                    , selection = Selection row row activeCell.column activeCell.column
                                }

                        -- right
                        39 ->
                            let
                                col =
                                    min model.numCols (activeCell.column + 1)
                            in
                                { model
                                    | activeCell = { activeCell | column = col }
                                    , selection = Selection activeCell.row activeCell.row col col
                                }

                        -- down
                        40 ->
                            let
                                row =
                                    min model.numRows (activeCell.row + 1)
                            in
                                { model
                                    | activeCell = { activeCell | row = row }
                                    , selection = Selection row row activeCell.column activeCell.column
                                }

                        _ ->
                            model

                DragStart row col ->
                    { model
                        | dragging = True
                        , activeCell = ActiveCell row col
                        , selection = Selection row row col col
                    }

                DragMove row col ->
                    case model.dragging of
                        False ->
                            model

                        True ->
                            { model
                                | selection =
                                    Selection
                                        (min activeCell.row row)
                                        (max activeCell.row row)
                                        (min activeCell.column col)
                                        (max activeCell.column col)
                            }

                DragEnd row col ->
                    { model | dragging = False }

                SelectAll ->
                    { model
                        | activeCell = ActiveCell 1 1
                        , selection =
                            Selection 1 model.numRows 1 model.numCols
                    }

                SelectColumn col ->
                    { model
                        | activeCell = ActiveCell 1 col
                        , selection =
                            Selection 1 model.numRows col col
                    }

                SelectRow row ->
                    { model
                        | activeCell = ActiveCell row 1
                        , selection =
                            Selection row row 1 model.numCols
                    }

                NoOp ->
                    model
    in
        updateHelper result



-- Subscriptions


port arrows : (KeyCode -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ arrows KeyDown
        ]



--
-- App


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

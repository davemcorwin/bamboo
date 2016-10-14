port module Main exposing (main)

import Html exposing (Attribute, Html, div, input, text)
import Html.App as App
import Html.Attributes exposing (id, class, style, type', value)
import Html.Events exposing (keyCode, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import Style exposing (..)
import StyleHelper exposing (..)
import String
import Keyboard exposing (KeyCode)
import Dom
import Task exposing (Task)
import Dict exposing (Dict)


-- Model


type alias Data =
    Dict ( Int, Int ) String


type alias Cell =
    { row : Int
    , column : Int
    }


type alias Range =
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
    , dragging : Bool
    , editing : Bool
    , activeCell : Cell
    , editCell : Cell
    , selection : Range
    , data : Data
    }


init : ( Model, Cmd Msg )
init =
    ( { numCols = 20
      , numRows = 20
      , dfltColWidth = 100
      , dfltRowHeight = 35
      , colHeaderColWidth = 50
      , dragging = False
      , editing = False
      , editCell = Cell 1 1
      , activeCell = Cell 1 1
      , selection = Range 1 1 1 1
      , data = Dict.empty
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


dataCell : Int -> Int -> Model -> Html Msg
dataCell row col model =
    input
        [ type' "text"
        , id ("input-" ++ (toString row) ++ "-" ++ (toString col))
        , class "data-cell"
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        , onDoubleClick (EditCell row col)
        , onMouseDown (DragStart row col)
        , onMouseUp (DragEnd row col)
        , onMouseEnter (DragMove row col)
        , onFocus (ActivateCell row col)
        , onInput (\content -> CellInput row col content)
        , value (Maybe.withDefault "" (Dict.get ( row, col ) model.data))
        ]
        []


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



-- Active Cell


activeCell : Int -> Int -> Html Msg
activeCell row col =
    div
        [ class "active-cell"
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        ]
        []



-- Selection Cell


selectionCell : Int -> Int -> Html Msg
selectionCell row col =
    div
        [ class "selection-cell"
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
data ({ selection } as model) =
    let
        cells =
            List.concatMap
                (\row ->
                    List.map
                        (\col -> dataCell row col model)
                        [1..model.numCols]
                )
                [1..model.numRows]

        notActiveCell row col =
            if row == model.activeCell.row && col == model.activeCell.column then
                activeCell row col
            else
                selectionCell row col

        selectionCells =
            List.concatMap
                (\row ->
                    List.map (notActiveCell row) [selection.startColumn..selection.endColumn]
                )
                [selection.startRow..selection.endRow]

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
    | ActivateCell Int Int
    | CellInput Int Int String
    | EditCell Int Int
    | DragEnd Int Int
    | DragMove Int Int
    | DragStart Int Int
    | FocusError Dom.Error
    | FocusSuccess ()
    | KeyDown KeyCode
    | SelectAll
    | SelectColumn Int
    | SelectRow Int


focusCmd : Cell -> Cmd Msg
focusCmd cell =
    Task.perform FocusError FocusSuccess (Dom.focus ("input-" ++ (toString cell.row) ++ "-" ++ (toString cell.column)))


activateCell : Cell -> Model -> Model
activateCell cell model =
    { model | activeCell = cell }


selectRange : Range -> Model -> Model
selectRange range model =
    { model | selection = range }


updateContent : Int -> Int -> String -> Model -> Model
updateContent row col content model =
    { model | data = (Dict.insert ( row, col ) content model.data) }


updateHelper : Model -> ( Model, Cmd Msg )
updateHelper model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ activeCell, selection } as model) =
    case msg of
        ActivateCell row col ->
            updateHelper
                (model
                    |> activateCell (Cell row col)
                    |> selectRange (Range row row col col)
                )

        CellInput row col content ->
            updateHelper
                (model
                    |> updateContent row col content
                )

        FocusError error ->
            updateHelper model

        FocusSuccess name ->
            updateHelper model

        EditCell row col ->
            updateHelper
                { model
                    | activeCell = Cell row col
                    , dragging = False
                    , editing = True
                    , selection = Range row row col col
                }

        KeyDown keyCode ->
            case keyCode of
                -- left
                37 ->
                    let
                        col =
                            max 1 (activeCell.column - 1)

                        cell =
                            Cell activeCell.row col
                    in
                        ( model
                            |> activateCell cell
                            |> selectRange (Range activeCell.row activeCell.row col col)
                        , focusCmd cell
                        )

                -- up
                38 ->
                    let
                        row =
                            max 1 (activeCell.row - 1)

                        cell =
                            Cell row activeCell.column
                    in
                        ( model
                            |> activateCell cell
                            |> selectRange (Range row row activeCell.column activeCell.column)
                        , focusCmd cell
                        )

                -- right
                39 ->
                    let
                        col =
                            min model.numCols (activeCell.column + 1)

                        cell =
                            Cell activeCell.row col
                    in
                        ( model
                            |> activateCell cell
                            |> selectRange (Range activeCell.row activeCell.row col col)
                        , focusCmd cell
                        )

                9 ->
                    let
                        col =
                            min model.numCols (activeCell.column + 1)

                        cell =
                            Cell activeCell.row col
                    in
                        ( model
                            |> activateCell cell
                            |> selectRange (Range activeCell.row activeCell.row col col)
                        , focusCmd cell
                        )

                -- down
                40 ->
                    let
                        row =
                            min model.numRows (activeCell.row + 1)

                        cell =
                            Cell row activeCell.column
                    in
                        ( model
                            |> activateCell cell
                            |> selectRange (Range row row activeCell.column activeCell.column)
                        , focusCmd cell
                        )

                _ ->
                    updateHelper model

        DragStart row col ->
            updateHelper
                { model
                    | dragging = True
                    , activeCell = Cell row col
                    , selection = Range row row col col
                }

        DragMove row col ->
            updateHelper
                (case model.dragging of
                    False ->
                        model

                    True ->
                        { model
                            | selection =
                                Range
                                    (min activeCell.row row)
                                    (max activeCell.row row)
                                    (min activeCell.column col)
                                    (max activeCell.column col)
                        }
                )

        DragEnd row col ->
            updateHelper
                { model | dragging = False }

        SelectAll ->
            updateHelper
                { model
                    | activeCell = Cell 1 1
                    , selection =
                        Range 1 model.numRows 1 model.numCols
                }

        SelectColumn col ->
            updateHelper
                { model
                    | activeCell = Cell 1 col
                    , selection =
                        Range 1 model.numRows col col
                }

        SelectRow row ->
            updateHelper
                { model
                    | activeCell = Cell row 1
                    , selection =
                        Range row row 1 model.numCols
                }

        NoOp ->
            updateHelper model



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

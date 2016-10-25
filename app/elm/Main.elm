port module Main exposing (main)

import Html exposing (Attribute, Html, div, input, text)
import Html.App as App
import Html.Lazy exposing (..)
import Html.Attributes exposing (id, class, contenteditable, style, type', value)
import Html.Events exposing (keyCode, onClick, onDoubleClick, onFocus, onInput, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, Options)
import Style exposing (..)
import StyleHelper exposing (..)
import String
import Dom
import Task exposing (Task)
import Dict exposing (Dict)


-- Model


rowsFun : Int -> Int -> String
rowsFun rowHeight numRows =
    rowHeight
        |> List.repeat numRows
        |> List.map px
        |> String.join " "


colsFun : Int -> Int -> String
colsFun colWidth numCols =
    colWidth
        |> List.repeat numCols
        |> List.map px
        |> String.join " "


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


type alias Defaults =
    { numCols : Int
    , numRows : Int
    , dfltColWidth : Int
    , dfltRowHeight : Int
    , colHeaderColWidth : Int
    , rows : String
    , columns : String
    }


type alias Model =
    { defaults : Defaults
    , dragging : Bool
    , editing : Bool
    , activeCell : Cell
    , selection : Range
    , data : Data
    }


init : ( Model, Cmd Msg )
init =
    let
        numCols =
            26

        numRows =
            100

        dfltColWidth =
            100

        dfltRowHeight =
            35

        defaults =
            { numCols = numCols
            , numRows = numRows
            , dfltColWidth = dfltColWidth
            , dfltRowHeight = dfltRowHeight
            , colHeaderColWidth = 50
            , rows = rowsFun dfltRowHeight numRows
            , columns = colsFun dfltColWidth numCols
            }
    in
        ( { defaults = defaults
          , dragging = False
          , editing = False
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


gridLayout : String -> String -> List (Html Msg) -> Html Msg
gridLayout rows cols children =
    div
        [ class "grid"
        , style
            [ gridTemplateColumns cols
            , gridTemplateRows rows
            ]
        ]
        children



-- Cells


dataCell : Int -> Int -> Cell -> Maybe String -> Html Msg
dataCell row col activeCell data =
    div
        [ class "data-cell"
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        , contenteditable (activeCell.row == row && activeCell.column == col)
        , onDoubleClick (EditCell row col)
        , onMouseDown (DragStart row col)
        , onMouseUp (DragEnd row col)
        , onMouseEnter (DragMove row col)
          -- , onFocus (ActivateCell row col)
          -- , onInput (\content -> CellInput row col content)
          -- , value (Maybe.withDefault "" data)
        ]
        []



-- dataCell : Int -> Int -> Maybe String -> Html Msg
-- dataCell row col data =
--     input
--         [ type' "text"
--           -- , id ("input-" ++ (toString row) ++ "-" ++ (toString col))
--         , class "data-cell"
--         , style
--             [ gridRow row row
--             , gridColumn col col
--             ]
--         , onDoubleClick (EditCell row col)
--         , onMouseDown (DragStart row col)
--         , onMouseUp (DragEnd row col)
--         , onMouseEnter (DragMove row col)
--         , onFocus (ActivateCell row col)
--         , onInput (\content -> CellInput row col content)
--         , value (Maybe.withDefault "" data)
--         ]
--         []


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


cornerCell : Defaults -> Html Msg
cornerCell defaults =
    let
        { colHeaderColWidth, dfltRowHeight } =
            defaults
    in
        div
            [ class "corner-cell"
            , style
                [ width (px (colHeaderColWidth + 1))
                , height (px (dfltRowHeight + 1))
                ]
            , onClick SelectAll
            ]
            []


selectionCell : Int -> Int -> Bool -> Html Msg
selectionCell row col isActive =
    div
        [ class
            (if isActive then
                "active-cell"
             else
                "selection-cell"
            )
        , style
            [ gridRow row row
            , gridColumn col col
            ]
        ]
        []



-- Headers


rowHeader : Defaults -> Html Msg
rowHeader defaults =
    let
        { colHeaderColWidth, columns, dfltColWidth, dfltRowHeight, numCols } =
            defaults

        cells =
            [1..numCols]
                |> List.map (\col -> headerCell 1 col (alpha (col - 1)) (SelectColumn col))
    in
        div
            [ class "row-header"
            , style
                [ width (px ((dfltColWidth + 1) * numCols + colHeaderColWidth + 1))
                , height (px (dfltRowHeight + 1))
                , marginLeft (px (dfltColWidth // 2 + 1))
                ]
            ]
            [ gridLayout (px dfltRowHeight) columns cells ]


colHeader : Defaults -> Html Msg
colHeader defaults =
    let
        { colHeaderColWidth, dfltColWidth, dfltRowHeight, numRows, rows } =
            defaults

        cells =
            [1..numRows]
                |> List.map (\row -> headerCell row 1 (toString row) (SelectRow row))
    in
        div
            [ class "col-header"
            , style
                [ height (px ((dfltRowHeight) * (numRows + 1)))
                , width (px (colHeaderColWidth + 1))
                ]
            ]
            [ gridLayout rows (px (dfltColWidth // 2)) cells ]



-- Ranges


dataCells : Cell -> Defaults -> Data -> List (Html Msg)
dataCells activeCell defaults data =
    let
        { dfltColWidth, dfltRowHeight, numCols, numRows, rows, columns } =
            defaults
    in
        List.concatMap
            (\row ->
                List.map
                    (\col -> dataCell row col activeCell (Dict.get ( row, col ) data))
                    [1..numCols]
            )
            [1..numRows]


selectionCells : Cell -> Range -> List (Html Msg)
selectionCells activeCell selection =
    let
        { endColumn, endRow, startColumn, startRow } =
            selection
    in
        List.concatMap
            (\row ->
                List.map
                    (\col ->
                        lazy3 selectionCell row col (row == activeCell.row && col == activeCell.column)
                    )
                    [startColumn..endColumn]
            )
            [startRow..endRow]


selectionRange : Range -> Html Msg
selectionRange selection =
    let
        { endColumn, endRow, startColumn, startRow } =
            selection
    in
        div
            [ class "selection-range"
            , style
                [ gridRow startRow (endRow + 1)
                , gridColumn startColumn (endColumn + 1)
                ]
            ]
            []



-- Main


foo : Model -> Html Msg
foo model =
    let
        { activeCell, data, defaults, selection } =
            model

        { columns, dfltColWidth, dfltRowHeight, rows } =
            defaults
    in
        div
            [ class "data"
            , style
                [ left (px ((dfltColWidth // 2) + 1))
                , top (px (dfltRowHeight + 1))
                ]
            ]
            [ gridLayout rows
                columns
                (List.concat
                    [ dataCells activeCell defaults data
                    , selectionCells activeCell selection
                    , [ selectionRange selection ]
                    ]
                )
            ]



-- Sheet


sheet : Model -> Html Msg
sheet model =
    let
        { activeCell, data, defaults, selection } =
            model
    in
        div
            [ id "sheet"
            , style
                [ width (px ((defaults.dfltColWidth + 1) * defaults.numCols + defaults.colHeaderColWidth + 1))
                , height (px ((defaults.dfltRowHeight + 1) * (defaults.numRows + 1)))
                ]
            ]
            [ lazy foo model
            , lazy cornerCell defaults
            , lazy rowHeader defaults
            , lazy colHeader defaults
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
    | KeyDown ( String, Bool )
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

        KeyDown ( key, shiftKey ) ->
            let
                cell =
                    case key of
                        "ArrowLeft" ->
                            Cell activeCell.row (max 1 (activeCell.column - 1))

                        "ArrowUp" ->
                            Cell (max 1 (activeCell.row - 1)) activeCell.column

                        "ArrowRight" ->
                            Cell activeCell.row (min model.defaults.numCols (activeCell.column + 1))

                        "Tab" ->
                            case shiftKey of
                                True ->
                                    Cell activeCell.row (max 1 (activeCell.column - 1))

                                False ->
                                    Cell activeCell.row (min model.defaults.numCols (activeCell.column + 1))

                        "ArrowDown" ->
                            Cell (min model.defaults.numRows (activeCell.row + 1)) activeCell.column

                        _ ->
                            activeCell
            in
                updateHelper
                    { model
                        | activeCell = cell
                        , selection = Range cell.row cell.row cell.column cell.column
                    }

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
                        Range 1 model.defaults.numRows 1 model.defaults.numCols
                }

        SelectColumn col ->
            updateHelper
                { model
                    | activeCell = Cell 1 col
                    , selection =
                        Range 1 model.defaults.numRows col col
                }

        SelectRow row ->
            updateHelper
                { model
                    | activeCell = Cell row 1
                    , selection =
                        Range row row 1 model.defaults.numCols
                }

        NoOp ->
            updateHelper model



-- Subscriptions


port keys : (( String, Bool ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ keys KeyDown
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

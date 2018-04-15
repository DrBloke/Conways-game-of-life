module Main exposing (..)

import Html exposing (Html, button, div, fieldset, input, label, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)
import List.Extra exposing (lift2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { grid : List Cell
    , rulesOfTheGame : Rules
    , showGridLines : Bool
    , zoomLevel : Int
    , inProgress : Bool
    , generationTime : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { grid = defaultGrid
      , rulesOfTheGame = Rules 2 3 3 Bounded
      , showGridLines = True
      , zoomLevel = 1
      , inProgress = False
      , generationTime = 1000
      }
    , Cmd.none
    )


type alias Cell =
    { coords : Coords
    , status : CellStatus
    , color : Maybe String
    }


type alias Coords =
    ( Int, Int )


type CellStatus
    = Dead
    | Alive


type alias Rules =
    { minNeighbours : Int
    , maxNeighbours : Int
    , spawnNeighbours : Int
    , edgeRule : EdgeRule
    }


type EdgeRule
    = Infinite
    | Bounded
    | Escape
    | Toroid


type Pattern
    = Block
    | Beehive
    | Blinker
    | Toad
    | Beacon
    | Pentadecathlon
    | Glider
    | RPentomino
    | Diehard
    | Acorn


type Msg
    = CellClick Coords
    | ToggleInProgress
    | UpdateGameState
    | InputGameState
    | OutputGameState
    | ToggleView
    | ToggleGridLines
    | ChanageZoomLevel Int
    | ChangeGenerationTime Int
    | ChangeRules Rules
    | Select Pattern
    | PlacePatternOnGrid Pattern Coords


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick coords ->
            ( { model | grid = List.map (toggleClickedCellState coords) model.grid }
            , Cmd.none
            )

        UpdateGameState ->
            ( { model | grid = List.map (applyGameRules model.grid) model.grid }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



--update helpers (don't use outside of update scope)


toggleClickedCellState : Coords -> Cell -> Cell
toggleClickedCellState clickedCoords cell =
    if cell.coords == clickedCoords then
        case cell.status of
            Alive ->
                { cell | status = Dead }

            Dead ->
                { cell | status = Alive }
    else
        cell


applyGameRules : List Cell -> Cell -> Cell
applyGameRules grid cell =
    let
        numberOfLiveNeighbours =
            List.sum (lift2 isInGridAndIsLive (getNeighboursCoords cell.coords) grid)
    in
        case cell.status of
            Alive ->
                if numberOfLiveNeighbours < 2 then
                    { cell | status = Dead }
                else if numberOfLiveNeighbours > 3 then
                    { cell | status = Dead }
                else
                    cell

            Dead ->
                if numberOfLiveNeighbours == 3 then
                    { cell | status = Alive }
                else
                    cell


isInGridAndIsLive : Coords -> Cell -> Int
isInGridAndIsLive neighbourCoords gridCell =
    if neighbourCoords == gridCell.coords && gridCell.status == Alive then
        1
    else
        0


getNeighboursCoords : Coords -> List Coords
getNeighboursCoords ( x, y ) =
    ( x - 1, y - 1 )
        :: ( x, y - 1 )
        :: ( x + 1, y - 1 )
        :: ( x - 1, y )
        :: ( x + 1, y )
        :: ( x - 1, y + 1 )
        :: ( x, y + 1 )
        :: ( x + 1, y + 1 )
        :: []



--View


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ width <| toString 1000
            , height <| toString 500
            , viewBox (" 0 0 1000 500")
            ]
            (drawGrid model.grid)
        , button [ Html.Events.onClick UpdateGameState ] [ Html.text "step" ]
        ]



--View functions


drawGrid : List Cell -> List (Svg Msg)
drawGrid grid =
    List.map renderCell grid


renderCell : Cell -> Svg Msg
renderCell cell =
    case cell.status of
        Dead ->
            rect [ x (toString <| ((Tuple.first cell.coords) * 100) - 100), y (toString <| ((Tuple.second cell.coords) * 100) - 100), width "100", height "100", fill (Maybe.withDefault "#ffffff" cell.color), stroke "#000000", strokeWidth "1", Svg.Events.onClick (CellClick cell.coords) ] []

        Alive ->
            rect [ x (toString <| ((Tuple.first cell.coords) * 100) - 100), y (toString <| ((Tuple.second cell.coords) * 100) - 100), width "100", height "100", fill (Maybe.withDefault "#000000" cell.color), stroke "#000000", strokeWidth "1", Svg.Events.onClick (CellClick cell.coords) ] []


defaultGrid : List Cell
defaultGrid =
    cartesianProduct (range 1 10) (range 1 5)
        |> List.map (\( x, y ) -> (Cell ( x, y ) Dead Nothing))


cartesianProduct xs ys =
    lift2 (,) xs ys



-- or this: List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

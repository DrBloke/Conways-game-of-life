module Main exposing (..)

import Html exposing (Html, button, div, fieldset, input, label, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (..)
import List.Extra exposing (lift2, unique)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (..)
import Material.Icons.Av exposing (skip_next, play_circle_filled, pause_circle_filled)
import Color


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { grid : List Coords
    , liveCells : List Coords
    , rulesOfTheGame : Rules
    , showGridLines : Bool
    , zoomLevel : Int
    , inProgress : Bool
    , generationTime : Int
    , generations : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { grid = emptyGrid
      , liveCells = []
      , rulesOfTheGame = Rules 2 3 3 Bounded
      , showGridLines = True
      , zoomLevel = 1
      , inProgress = False
      , generationTime = 1000
      , generations = 0
      }
    , Cmd.none
    )


type alias Coords =
    ( Int, Int )


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
    | Tick Bool Time
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
            ( { model | liveCells = toggleClickedCellState coords model.liveCells }
            , Cmd.none
            )

        UpdateGameState ->
            ( { model
                | liveCells = applyGameRules model.liveCells
                , generations = model.generations + 1
              }
            , Cmd.none
            )

        Tick inProgress time ->
            if inProgress then
                update UpdateGameState model
            else
                ( model, Cmd.none )

        ToggleInProgress ->
            ( { model | inProgress = not model.inProgress }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



--update helpers (don't use outside of update scope)


toggleClickedCellState : Coords -> List Coords -> List Coords
toggleClickedCellState clickedCoords liveCells =
    if List.member clickedCoords liveCells then
        List.filter (\x -> x /= clickedCoords) liveCells
    else
        clickedCoords :: liveCells


applyGameRules : List Coords -> List Coords
applyGameRules liveCells =
    let
        --the 1 and 4 need to be replaced with Rules.max and min
        liveCellsThatContiueToLive =
            List.filter (\coords -> (numLiveNeighbours liveCells coords) > 1 && (numLiveNeighbours liveCells coords) < 4) liveCells

        deadCellsThatComeToLife =
            (List.foldl (getNeighboursIfDead liveCells) [] liveCells)
                |> List.filter (\coords -> (numLiveNeighbours liveCells coords) == 3)
    in
        List.append liveCellsThatContiueToLive deadCellsThatComeToLife
            |> unique


numLiveNeighbours : List Coords -> Coords -> Int
numLiveNeighbours liveCells coords =
    List.length (liveNeighboursOfCell liveCells coords)


liveNeighboursOfCell : List Coords -> Coords -> List Coords
liveNeighboursOfCell liveCells coords =
    coords
        |> getNeighboursCoords
        |> List.filter (flip List.member liveCells)


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


getNeighboursIfDead : List Coords -> Coords -> List Coords -> List Coords
getNeighboursIfDead liveCells cell deadNeighbours =
    let
        neighbours =
            getNeighboursCoords cell

        newDeadNeighbours =
            List.filter (\thisNeighbour -> not <| List.member thisNeighbour liveCells) neighbours
    in
        List.append newDeadNeighbours deadNeighbours



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
            ((drawGrid model.grid)
                ++ (drawLiveCells model.liveCells)
            )
        , Html.text <| toString model.generations
        , playOrPauseButton model.inProgress
        , skipNextButton
        ]



--View functions


drawGrid : List Coords -> List (Svg Msg)
drawGrid grid =
    List.map renderEmptyCell grid


drawLiveCells : List Coords -> List (Svg Msg)
drawLiveCells liveCells =
    List.map renderLiveCell liveCells


playOrPauseButton : Bool -> Svg Msg
playOrPauseButton inProgress =
    let
        button =
            if inProgress then
                pause_circle_filled
            else
                play_circle_filled
    in
        svg
            [ width <| toString 50
            , height <| toString 50
            , Svg.Events.onClick ToggleInProgress
            ]
            [ button Color.black 50 ]


skipNextButton : Svg Msg
skipNextButton =
    svg
        [ width <| toString 50
        , height <| toString 50
        , Svg.Events.onClick UpdateGameState
        ]
        [ skip_next Color.black 50 ]


renderEmptyCell : Coords -> Svg Msg
renderEmptyCell coords =
    rect [ x (toString <| ((Tuple.first coords) * 50) - 50), y (toString <| ((Tuple.second coords) * 50) - 50), width "50", height "50", fill "#ffffff", stroke "#000000", strokeWidth "1", Svg.Events.onClick (CellClick coords) ] []


renderLiveCell : Coords -> Svg Msg
renderLiveCell coords =
    rect [ x (toString <| ((Tuple.first coords) * 50) - 50), y (toString <| ((Tuple.second coords) * 50) - 50), width "50", height "50", fill "#000000", Svg.Events.onClick (CellClick coords) ] []


emptyGrid : List Coords
emptyGrid =
    cartesianProduct (range 1 20) (range 1 10)


cartesianProduct xs ys =
    lift2 (,) xs ys



-- or this: List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * millisecond) (Tick model.inProgress)

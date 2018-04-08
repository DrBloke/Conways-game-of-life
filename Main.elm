module Main exposing (..)

import Html exposing (Html, button, div, fieldset, input, label, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra
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
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ width <| toString 200
            , height <| toString 100
            , viewBox (" 0 0 200 100")
            ]
            (drawGrid model.grid)
        ]



--View functions


drawGrid : List Cell -> List (Svg msg)
drawGrid grid =
    List.map renderCell grid


renderCell : Cell -> Svg msg
renderCell cell =
    case cell.status of
        Dead ->
            rect [ x (toString <| (Tuple.first cell.coords) * 100), y (toString <| (Tuple.second cell.coords) * 100), width "100", height "100", fill (Maybe.withDefault "#ffffff" cell.color), stroke "#000000", strokeWidth "1" ] []

        Alive ->
            rect [ x (toString <| (Tuple.first cell.coords) * 100), y (toString <| (Tuple.second cell.coords) * 100), width "100", height "100", fill (Maybe.withDefault "#ffffff" cell.color), stroke "#000000", strokeWidth "1" ] []


defaultGrid : List Cell
defaultGrid =
    [ (Cell ( 0, 0 ) Dead Nothing)
    , (Cell ( 1, 0 ) Alive <| Just "#000000")
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

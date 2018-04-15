module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)
import List.Extra exposing (..)


suite : Test
suite =
    describe "Applying the rules of the game"
        [ test "gets a list of neighbours" <|
            \_ ->
                Expect.equal (getNeighboursCoords ( 2, 2 ))
                    [ ( 1, 1 )
                    , ( 2, 1 )
                    , ( 3, 1 )
                    , ( 1, 2 )
                    , ( 3, 2 )
                    , ( 1, 3 )
                    , ( 2, 3 )
                    , ( 3, 3 )
                    ]
        , test "Knows when neighbour is live and returns 1 for the total" <|
            \_ ->
                Expect.equal
                    (isInGridAndIsLive ( 1, 1 ) (Cell ( 1, 1 ) Alive Nothing))
                    1
        , test "Knows when neighbour is dead and returns 0 for the total" <|
            \_ ->
                Expect.equal
                    (isInGridAndIsLive ( 1, 1 ) (Cell ( 1, 1 ) Dead Nothing))
                    0
        , test "This is not the matching coordinate so don't add to the total" <|
            \_ ->
                Expect.equal
                    (isInGridAndIsLive ( 2, 1 ) (Cell ( 1, 1 ) Alive Nothing))
                    0
        , test "Compares each neighbour with the active grid" <|
            \_ ->
                let
                    neighbours =
                        [ ( 1, 1 )
                        , ( 2, 1 )
                        , ( 3, 1 )
                        , ( 1, 2 )
                        , ( 3, 2 )
                        , ( 1, 3 )
                        , ( 2, 3 )
                        , ( 3, 3 )
                        ]

                    grid =
                        [ (Cell ( 1, 1 ) Alive Nothing)
                        , (Cell ( 2, 1 ) Alive Nothing)
                        , (Cell ( 1, 2 ) Alive Nothing)
                        , (Cell ( 2, 2 ) Alive Nothing)
                        ]
                in
                    Expect.equal
                        (lift2 isInGridAndIsLive neighbours grid)
                        [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
        , test "Does it work out the number of live neighbours" <|
            \_ ->
                Expect.equal
                    (List.sum
                        (lift2 isInGridAndIsLive (getNeighboursCoords ( 1, 1 )) [ (Cell ( 1, 1 ) Alive Nothing), (Cell ( 2, 1 ) Alive Nothing), (Cell ( 1, 2 ) Alive Nothing), (Cell ( 2, 2 ) Alive Nothing) ])
                    )
                    3
        ]

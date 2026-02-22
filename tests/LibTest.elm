module LibTest exposing
    ( testDoubleFirst1
    , testDoubleFirst2
    , testFilterTrianglesByType
    , testFilterValidTriangles
    , testIndexOf
    , testMakeTriangles
    , testParseValues
    , testSortTrianglesByArea
    , testSumTrianglesArea
    )

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Lib exposing (..)
import Test exposing (..)


testMakeTriangles : Test
testMakeTriangles =
    describe "makeTriangles"
        [ test "creates triangles" <|
            \_ ->
                let
                    triangles =
                        makeTriangles
                            [ 1, 2, 3, 1, 5 ]
                            [ 4, 5, 6, 4 ]
                            [ 7, 8, 9 ]
                in
                Expect.equal triangles [ Triangle 1 4 7, Triangle 2 5 8, Triangle 3 6 9 ]
        , test "returns empty list when all lists are empty" <|
            \_ -> Expect.equal [] <| makeTriangles [] [] []
        , test "returns empty list when first list is empty" <|
            \_ -> Expect.equal [] <| makeTriangles [] [ 1, 2 ] [ 3, 4 ]
        , test "creates single triangle from single-element lists" <|
            \_ -> Expect.equal [ Triangle 1 2 3 ] <| makeTriangles [ 1 ] [ 2 ] [ 3 ]
        , test "stops at shortest list" <|
            \_ ->
                Expect.equal [ Triangle 10 20 30 ] <|
                    makeTriangles [ 10, 99 ] [ 20, 99 ] [ 30 ]
        , test "handles float values" <|
            \_ ->
                Expect.equal [ Triangle 1.5 2.5 3.5 ] <|
                    makeTriangles [ 1.5 ] [ 2.5 ] [ 3.5 ]
        , test "returns empty when second list is empty" <|
            \_ -> Expect.equal [] <| makeTriangles [ 1, 2 ] [] [ 3, 4 ]
        ]


testFilterValidTriangles : Test
testFilterValidTriangles =
    describe "filterValidTriangles"
        [ test "filter triangles" <|
            \_ ->
                let
                    triangles =
                        filterValidTriangles <|
                            makeTriangles
                                [ 10, 45, 18, 0, -8, 6, -3, 1, -5, 3, 4 ]
                                [ 12, 10, 18, 6, -9, 8, -3, 2, 6, 2 ]
                                [ 22, 45, 18, 9, -7, 10, -3, 5, 12 ]
                in
                Expect.equal triangles
                    [ Triangle 45 10 45, Triangle 18 18 18, Triangle 6 8 10 ]
        , test "returns empty list for empty input" <|
            \_ -> Expect.equal [] <| filterValidTriangles []
        , test "returns empty when all triangles invalid" <|
            \_ ->
                Expect.equal [] <|
                    filterValidTriangles
                        [ Triangle 1 2 10, Triangle 0 0 0, Triangle -1 2 3 ]
        , test "keeps all when all valid" <|
            \_ ->
                let
                    valid =
                        [ Triangle 3 4 5, Triangle 5 5 8 ]
                in
                Expect.equal valid <| filterValidTriangles valid
        , test "rejects triangle where sum of two sides equals third" <|
            \_ ->
                Expect.equal [] <|
                    filterValidTriangles [ Triangle 1 2 3 ]
        , test "rejects triangle with zero side" <|
            \_ ->
                Expect.equal [] <|
                    filterValidTriangles [ Triangle 3 4 0 ]
        ]


testFilterTrianglesByType : Test
testFilterTrianglesByType =
    describe "filterTrianglesByType"
        [ test "filter right-angled triangles" <|
            \_ ->
                let
                    triangles =
                        filterTrianglesByType RightAngled <|
                            makeTriangles
                                [ 4, 12, 12, 0, 7, 5, 3, 5 ]
                                [ 3, 56, 5, 6, 24, 6, 5, 4 ]
                                [ 5, -6, 13, 7, 25, 7, 4, 3 ]
                in
                Expect.equal triangles
                    [ Triangle 4 3 5, Triangle 12 5 13, Triangle 7 24 25, Triangle 3 5 4, Triangle 5 4 3 ]
        , test "filter isosceles triangles" <|
            \_ ->
                let
                    triangles =
                        filterTrianglesByType Isosceles <|
                            makeTriangles
                                [ 5, 323, 54, -8, 0, 6, 7, 5 ]
                                [ 23, 128, 54, 9, 2, 6, 7, 7 ]
                                [ 23, 462, 12, 9, 3, 6, 5, 5 ]
                in
                Expect.equal triangles
                    [ Triangle 5 23 23, Triangle 54 54 12, Triangle 6 6 6, Triangle 7 7 5, Triangle 5 7 5 ]
        , test "filter equiliteral triangles" <|
            \_ ->
                let
                    triangles =
                        filterTrianglesByType Equilateral <|
                            makeTriangles
                                [ 0, 7, -2, 3, 123, 121, -3 ]
                                [ 0, 7, -2, 6, 5, 121, 5 ]
                                [ 0, 7, -2, 9, 8, 121, 5 ]
                in
                Expect.equal triangles
                    [ Triangle 7 7 7, Triangle 121 121 121 ]
        , test "returns empty for empty input" <|
            \_ -> Expect.equal [] <| filterTrianglesByType RightAngled []
        , test "returns empty when no triangles match type" <|
            \_ ->
                Expect.equal [] <|
                    filterTrianglesByType Equilateral
                        [ Triangle 3 4 5, Triangle 5 12 13 ]
        , test "right-angled triangle 8-15-17" <|
            \_ ->
                Expect.equal [ Triangle 8 15 17 ] <|
                    filterTrianglesByType RightAngled [ Triangle 8 15 17 ]
        , test "isosceles with two equal shorter sides" <|
            \_ ->
                Expect.equal [ Triangle 5 5 8 ] <|
                    filterTrianglesByType Isosceles [ Triangle 5 5 8 ]
        ]


testSortTrianglesByArea : Test
testSortTrianglesByArea =
    describe "sortTrianglesByArea"
        [ test "sorts" <|
            \_ ->
                let
                    triangles =
                        [ Triangle 2 4 3
                        , Triangle 10 12 21
                        , Triangle 5 6 7
                        , Triangle 6 7 4
                        ]

                    sorted =
                        [ Triangle 2 4 3
                        , Triangle 6 7 4
                        , Triangle 5 6 7
                        , Triangle 10 12 21
                        ]
                in
                Expect.equal sorted <| sortTrianglesByArea triangles
        , test "returns empty list for empty input" <|
            \_ -> Expect.equal [] <| sortTrianglesByArea []
        , test "returns same list for single triangle" <|
            \_ ->
                let
                    one =
                        [ Triangle 5 5 5 ]
                in
                Expect.equal one <| sortTrianglesByArea one
        , test "handles triangles with equal area" <|
            \_ ->
                let
                    triangles =
                        [ Triangle 3 4 5, Triangle 4 3 5 ]

                    sorted =
                        sortTrianglesByArea triangles
                in
                Expect.equal 2 (List.length sorted)
        ]


testSumTrianglesArea : Test
testSumTrianglesArea =
    describe "sumTriangleArea"
        [ test "sums the area" <|
            \_ ->
                let
                    triangles =
                        [ Triangle 2 4 3
                        , Triangle 10 12 21
                        , Triangle 5 6 7
                        , Triangle 6 7 4
                        ]
                in
                Expect.within (Absolute 0.001) 63.848 <| sumTrianglesArea triangles
        , test "sums empty list" <|
            \_ -> Expect.equal 0 <| sumTrianglesArea []
        , test "sums single triangle" <|
            \_ ->
                Expect.within (Absolute 0.001) 6 <|
                    sumTrianglesArea [ Triangle 3 4 5 ]
        , test "sums two triangles" <|
            \_ ->
                Expect.within (Absolute 0.001) 12 <|
                    sumTrianglesArea [ Triangle 3 4 5, Triangle 3 4 5 ]
        , test "equilateral triangle area" <|
            \_ ->
                Expect.within (Absolute 0.001) 10.825 <|
                    sumTrianglesArea [ Triangle 5 5 5 ]
        ]


testDoubleFirst1 : Test
testDoubleFirst1 =
    testDoubleFirst "doubleFirst1" doubleFirst1


testDoubleFirst2 : Test
testDoubleFirst2 =
    testDoubleFirst "doubleFirst2" doubleFirst2


testDoubleFirst : String -> (Maybe (List Int) -> Int) -> Test
testDoubleFirst name doubleFirst =
    describe name
        [ test "doubles first number" <|
            \_ -> Expect.equal 4 <| doubleFirst <| Just [ 2, 5, 1 ]
        , test "doubles negative first number" <|
            \_ -> Expect.equal -18 <| doubleFirst <| Just [ -9, 0 ]
        , test "doubles zero to zero" <|
            \_ -> Expect.equal 0 <| doubleFirst <| Just [ 0, 14, 55, 31 ]
        , test "returns 0 for empty list" <|
            \_ -> Expect.equal 0 <| doubleFirst <| Just []
        , test "returns 0 when list is Nothing" <|
            \_ -> Expect.equal 0 <| doubleFirst Nothing
        , test "single element list" <|
            \_ -> Expect.equal 42 <| doubleFirst <| Just [ 21 ]
        , test "large number" <|
            \_ -> Expect.equal 2000 <| doubleFirst <| Just [ 1000, 2, 3 ]
        ]


testParseValues : Test
testParseValues =
    describe "parseValues"
        [ parametrized
            [ ( "a = 12.5\nb = 6.7\n\ntx = 134.76"
              , [ ( "a", 12.5 ), ( "b", 6.7 ), ( "tx", 134.76 ) ]
              )
            , ( "x=5", [ ( "x", 5 ) ] )
            , ( "y           =   78\n\n\n\n\n\n     z=\t\t65.3"
              , [ ( "y", 78 ), ( "z", 65.3 ) ]
              )
            , ( "", [] )
            ]
            "works for valid data"
          <|
            \( source, expectedData ) ->
                case parseValues source of
                    Err err ->
                        Expect.fail err

                    Ok parsedValues ->
                        Expect.equalDicts parsedValues <| Dict.fromList expectedData
        , test "returns an error for invalid string" <|
            \_ ->
                Expect.err <| parseValues "a = abc; c = 15"
        , test "parses integer values as floats" <|
            \_ ->
                case parseValues "n = 42" of
                    Ok d ->
                        Expect.equal (Just 42) <| Dict.get "n" d

                    Err e ->
                        Expect.fail e
        , test "parses decimal with leading zero" <|
            \_ ->
                case parseValues "val = 0.5" of
                    Ok d ->
                        Expect.within (Absolute 0.001) 0.5 (Maybe.withDefault 0 <| Dict.get "val" d)

                    Err e ->
                        Expect.fail e
        , test "parses zero value" <|
            \_ ->
                case parseValues "zero = 0" of
                    Ok d ->
                        Expect.equal (Just 0) <| Dict.get "zero" d

                    Err e ->
                        Expect.fail e
        , test "parses multiple decimal places" <|
            \_ ->
                case parseValues "pi = 3.14159" of
                    Ok d ->
                        Expect.within (Absolute 0.00001) 3.14159 (Maybe.withDefault 0 <| Dict.get "pi" d)

                    Err e ->
                        Expect.fail e
        , test "parses multiple pairs without newlines" <|
            \_ ->
                case parseValues "x = 1 y = 2 z = 3" of
                    Ok d ->
                        Expect.all
                            [ \dict -> Expect.equal (Just 1) (Dict.get "x" dict)
                            , \dict -> Expect.equal (Just 2) (Dict.get "y" dict)
                            , \dict -> Expect.equal (Just 3) (Dict.get "z" dict)
                            ]
                            d

                    Err e ->
                        Expect.fail e
        , test "returns error when value is not a number" <|
            \_ ->
                Expect.err <| parseValues "x = 12abc"
        , test "returns error for invalid float" <|
            \_ ->
                Expect.err <| parseValues "key = 1e"
        ]


testIndexOf : Test
testIndexOf =
    describe "indexOf"
        [ test "returns correct index for ints" <|
            \_ -> Expect.equal (Just 1) <| indexOf 2 [ 1, 2, 3, 4, 5, 6 ]
        , test "returns nothing for ints" <|
            \_ -> Expect.equal Nothing <| indexOf 5 [ 1, 1, 1, 1 ]
        , test "returns correct index for strings" <|
            \_ -> Expect.equal (Just 2) <| indexOf "abc" [ "bca", "cbc", "abc", "acc" ]
        , test "returns Just 0 for first element" <|
            \_ -> Expect.equal (Just 0) <| indexOf 10 [ 10, 20, 30 ]
        , test "returns index of last element" <|
            \_ -> Expect.equal (Just 3) <| indexOf 40 [ 10, 20, 30, 40 ]
        , test "returns Nothing for empty list" <|
            \_ -> Expect.equal Nothing <| indexOf 1 []
        , test "returns first occurrence for duplicates" <|
            \_ -> Expect.equal (Just 1) <| indexOf 2 [ 1, 2, 2, 2 ]
        , test "works with Bool" <|
            \_ -> Expect.equal (Just 2) <| indexOf True [ False, False, True ]
        , test "works with single-element list" <|
            \_ -> Expect.equal (Just 0) <| indexOf 7 [ 7 ]
        ]


parametrized : List a -> String -> (a -> Expectation) -> Test
parametrized fixtures desc testFunction =
    describe desc
        (List.indexedMap (\i f -> test (desc ++ " " ++ String.fromInt i) (\_ -> testFunction f)) fixtures)

module Hashing exposing (..)

import Css exposing (..)
import Css.Class as Hashed
import Css.Class.Internal exposing (toClassname)
import Expect exposing (Expectation)
import Test exposing (..)


testToClassname : Test
testToClassname =
    Test.concat
        [ test "same attributes get the same classname" <|
            \_ ->
                toClassname [ display none ]
                    |> Expect.equal (toClassname [ display none ])
        , test "different attribute names yield different classnames" <|
            \_ ->
                toClassname [ margin auto ]
                    |> Expect.notEqual (toClassname [ left auto ])
        , test "different attribute values yield different classnames" <|
            \_ ->
                toClassname [ color (rgb 0 0 0) ]
                    |> Expect.notEqual (toClassname [ color (rgb 1 0 0) ])
        , test "pseudoclasses affect the classname" <|
            \_ ->
                toClassname [ textDecoration none, hover [ textDecoration underline ] ]
                    |> Expect.notEqual (toClassname [ textDecoration none ])
        ]


compiling : Test
compiling =
    Test.concat
        [ test "compiling one attribute" <|
            \_ ->
                [ display none ]
                    |> compileClasses
                    |> Expect.equal "._17007cd8 {\n    display: none;\n}"
        , test "compiling two attributes" <|
            \_ ->
                [ display none, color (rgb 5 10 15) ]
                    |> compileClasses
                    |> Expect.equal "._9e31ce74 {\n    display: none;\n    color: rgb(5, 10, 15);\n}"
        , test "compiling three attributes" <|
            \_ ->
                [ float left, display none, color (rgb 5 10 15) ]
                    |> compileClasses
                    |> Expect.equal "._993710c8 {\n    float: left;\n    display: none;\n    color: rgb(5, 10, 15);\n}"
        , test "pseudoclasses get the hash" <|
            \_ ->
                [ float left, textDecoration none, hover [ textDecoration underline, color (rgb 5 10 15) ] ]
                    |> compileClasses
                    |> Expect.equal "._3c9fd6bc {\n    float: left;\n    text-decoration: none;\n}\n\n._3c9fd6bc:hover {\n    text-decoration: underline;\n    color: rgb(5, 10, 15);\n}"
        , test "empty style list still means no output" <|
            \_ ->
                []
                    |> compileClasses
                    |> Expect.equal ""
        ]


compileClasses : List Style -> String
compileClasses list =
    Hashed.class list
        |> List.singleton
        |> Hashed.compile
        |> .css

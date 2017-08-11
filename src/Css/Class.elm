module Css.Class exposing (Class, class, compile, toAttribute, toStyleNode, with)

{-| Generate CSS class names by hashing their styles.


## Creating Classes

@docs Class, class


## Adding Classes to Html Values

@docs toAttribute, with


## Adding Styles to the Page

You need to use either `toStyleNode` to render to a `<style>` on the
page, or `compile` to generate a `.css` file, but not both. Without doing this,
you'll have some generated classnames on your elements, but no CSS declarations
to back them up!

It only makes sense to use one of these. You don't want to have both a `<style>`
element on the page and a `.css` file. If you're not sure which to use, the
`<style>` element is easier to set up; you call it and use the `Html` it
returns in your program's `view` function.

@docs toStyleNode, compile

-}

import Css exposing (Snippet, Style)
import Css.Class.Internal as Internal
import FNV
import Hex
import Html exposing (Attribute, Html)
import Html.Attributes


{-| Styles scoped under an automatically-generated class.
-}
type Class
    = Class (List Css.Style) String


{-| Create an Class from CSS styles. Use [`toAttribute`](#toAttribute) to
turn it into a `class` attribute, and [`toStyleNode`](toStyleNode) for
how to turn it into

    import Css exposing (backgroundColor, rgb)
    import Css.Class as Hashed
    import Html exposing (Attribute, Html, text)
    import Html.Attributes exposing (class)

    warning : Class
    warning =
        -- Suppose this automatically generates a class of "fc4bde3a1"
        Hashed.class [ backgroundColor (rgb 128 12 12) ]

-}
class : List Style -> Class
class styles =
    Class styles (Internal.toClassname styles)


{-| A drop-in replacement for [`Css.compile`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css#compile) (or [`Css.File.compile`](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Css-File#compile))
which takes `List Class` instead of `List Stylesheet`.

The most common way to use this is with the `elm-css` command line tool,
to generate a `.css` file. Strart with the exapmle from the `elm-css` README,
but replace `Css.File.compile` with `Hashed.compile` - everything else works
exactly the same way!

    import Css.Class as Hashed
    import Css.File exposing (CssCompilerProgram, CssFileStructure)
    import MyCss

    port files : CssFileStructure -> Cmd msg

    fileStructure : CssFileStructure
    fileStructure =
        Css.File.toFileStructure
            [ ( "index.css", Hashed.compile [ MyCss.css ] ) ]

    main : CssCompilerProgram
    main =
        Css.File.compiler files fileStructure

-}
compile : List Class -> { css : String, warnings : List String }
compile autoclasses =
    autoclasses
        |> List.map toSnippet
        |> Css.stylesheet
        |> List.singleton
        |> Css.compile


{-| Render autoclasses to a `<style>` element.
-}
toStyleNode : List Class -> Html msg
toStyleNode autoclasses =
    autoclasses
        |> compile
        |> .css
        |> Html.text
        |> List.singleton
        |> Html.node "style" []


{-| Create an Attribute from a Class.
-}
toAttribute : Class -> Attribute msg
toAttribute (Class _ classname) =
    Html.Attributes.class classname


{-| Prepend a generated class to a `Html.node` function's attributes list.

    import Css exposing (backgroundColor, rgb)
    import Css.Class as Hashed
    import Html exposing (Attribute, Html, button, text)
    import Html.Attributes exposing (class)

    warning : Class
    warning =
        -- Let's assume this automatically generates a classname of "fc4bde3a1"
        Hashed.class [ backgroundColor (rgb 128 12 12) ]

    {-| Create a `button` variant that automatically includes the warning style.
    -}
    warningButton : List (Attribute msg) -> List (Html msg) -> Html msg
    warningButton =
        Hashed.with warning button

    confirmDeleteButton : Html msg
    confirmDeleteButton =
        -- Equivalent to:
        --
        -- button [ class "fc4bde3a1" ][ text "Confirm Deletion" ]
        warningButton [] [ text "Confirm Deletion" ]

Since `class` attributes "stack" in Elm
(e.g. `button [ class "fc4bde3a1", class "centered" ] []` is equivalent to
`button [ class "fc4bde3a1 centered" ] []`), this API permits further
customization of the element's styles, either by using the `style` attribute
or by stacking additional classes
(for example `warningButton [ class "centered" ] []`).

-}
with :
    Class
    -> (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
with autoclass makeElem attributes =
    makeElem (toAttribute autoclass :: attributes)



-- INTERNAL --


toSnippet : Class -> Snippet
toSnippet (Class styles classname) =
    Css.class classname styles


toClassname : List Css.Style -> String
toClassname styles =
    -- TODO Replace this comically inefficient implementation
    -- with crawling these union types and building up a hash along the way.
    styles
        |> Css.everything
        |> List.singleton
        |> Css.stylesheet
        |> List.singleton
        |> Css.compile
        |> .css
        |> FNV.hashString
        |> Hex.toString
        |> String.cons 'h'

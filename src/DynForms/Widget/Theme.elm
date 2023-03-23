module DynForms.Widget.Theme
    exposing
        ( textInputLabel
        , selectInputLabel
        , bottomLineWrapper
        , getBottomTextData
        , bottomText
        , wrapper
        , wrapperLarge
        )


import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (..)
import Form exposing (InputType(..), Msg(..), FieldState)
import DynForms.Field exposing (FieldInfo)


textInputLabel : FieldInfo -> FieldState e String -> Html Msg
textInputLabel info state =
    let
        label =
            case info.label of
                Just st ->
                    [ span [] [ text st ] ]

                Nothing ->
                    []
    in
    div
        [ classList
            [ ( "eti-text-input__label", True )
            , ( "eti-text-input__label--raised", state.hasFocus || state.value /= Just "" )
            , ( "eti-text-input__label--active", state.hasFocus )
            ]
        ]
        label

     
selectInputLabel : FieldInfo -> FieldState e String -> Html Msg
selectInputLabel info state =
    let
        label =
            case info.label of
                Just st ->
                    [ span [] [ text st ] ]

                Nothing ->
                    []
    in
    div
        [ classList
            [ ( "eti-text-input__label", True )
            , ( "eti-text-input__label--raised", state.value /= Just "" )
            , ( "eti-text-input__label--active", state.hasFocus )
            ]
        ]
        label


bottomLineWrapper : FieldState e a -> Html Msg
bottomLineWrapper state =
    div
        [ class "eti-bottom-line__container"
        ]
        [ div [ class "eti-bottom-line__relative-wrapper" ]
            [ div
                [ classList
                    [ ( "eti-bottom-line__highlighter", True )
                    , ( "eti-bottom-line__highlighter--active", state.hasFocus )
                    ]
                ]
                []
            ]
        ]


getBottomTextData : FieldInfo -> FieldState e a -> Maybe ( Attribute msg, String )
getBottomTextData info state =
    case state.error of
        --Just errorValue ->
            --Just ( class "eti-bottom-text eti-bottom-text--error", Debug.toString errorValue )

        --Nothing ->
        _ ->
            Maybe.map
                (\helperText ->
                    ( class "eti-bottom-text eti-bottom-text--helper", helperText )
                )
                info.help
        
bottomText : FieldInfo -> FieldState e a -> Html Msg
bottomText info state =
    Maybe.withDefault (div [] []) <|
        Maybe.map
            (\( classAttr, innerText ) -> div [ classAttr ] [ text innerText ])
            (getBottomTextData info state)


wrapper : FieldInfo -> FieldState e a -> Html Msg -> Html Msg
wrapper info state component =
    div
        [ classList
            [ ( "eti-container", True )
            , ( "eti-outline", True ) --Outline style
            , ( "eti-outline--focused", state.hasFocus )
            , ( "eti-outline--error", (state.error /= Nothing) )
            ]
        ]
        [ component
        , bottomLineWrapper state --Filled style
        , bottomText info state
        ]

wrapperLarge : FieldInfo -> FieldState e a -> Html Msg -> Html Msg
wrapperLarge info state component =
    div
        [ classList
            [ ( "eti-container", True )
            , ( "eti-container-large", True )
            , ( "eti-outline", True ) --Outline style
            , ( "eti-outline--focused", state.hasFocus )
            , ( "eti-outline--error", (state.error /= Nothing) )
            ]
        ]
        [ component
        , bottomLineWrapper state --Filled style
        , bottomText info state
        ]
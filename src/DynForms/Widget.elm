module DynForms.Widget
    exposing
        ( textInput
        , textAreaInput
        , dateInput
        , dropdownInput
        , checkboxInput
        , readOnlyInput
        , fileInput
        )

import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (..)
import Form exposing (InputType(..), Msg(..), FieldState)
import Form.Field as Field exposing (FieldValue(..))
import Form.Input exposing (Input)
import Json.Decode as Json
import String
import File exposing (File)

import DynForms exposing (Form, Msg(..))
import DynForms.Field exposing (FieldInfo)
import DynForms.Widget.Theme exposing (..)
import DynForms.Widget.DatePicker exposing (defaultSettings)
import DynForms.Widget.DateRender as DateRender


{-| Reference
-- Form --
type alias FieldState e a =
    { path : String
    , value : Maybe a
    , error : Maybe (ErrorValue e)
    , liveError : Maybe (ErrorValue e)
    , isDirty : Bool
    , isChanged : Bool
    , hasFocus : Bool
    }

-- Form.Input --
type alias Input e a =
    FieldState e a -> List (Attribute Msg) -> Html Msg
-}


textInput : FieldInfo -> Input e String
textInput info state attrs =
    let
        formAttrs =
            [ type_ "text"
            , class "eti-text-input__input"
            , value (state.value |> Maybe.withDefault "")
            , onInput (String >> Input state.path Text)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    div
        [ class "eti-text-input__wrapper"
        ]
        [ textInputLabel info state
        , input (formAttrs ++ attrs) []
        ]


textAreaInput : FieldInfo -> Input e String
textAreaInput info state attrs =
    let
        formAttrs =
            [ class "eti-text-input__input"
            , value (state.value |> Maybe.withDefault "")
            , onInput (String >> Input state.path Text)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    div
        [ class "eti-text-input__wrapper"
        ]
        [ textInputLabel info state
        , textarea (formAttrs ++ attrs) []
        ]
        

dateInput : Form -> FieldInfo -> FieldState e String -> List (Attribute Msg) -> Html Msg
dateInput form info state attrs =
    div
        [ class "eti-text-input__wrapper"
        ]
        [ selectInputLabel info state
            |> Html.map StateMsg
        , DateRender.view
            defaultSettings
            form.datePicker
            state
            attrs
        ]


dropdownInput : List ( String, String ) -> FieldInfo -> Input e String
dropdownInput options info state attrs =
    let
        formAttrs =
            [ class "eti-text-input__input"
            , style "cursor" "pointer"
            , on "change" (targetValue |> Json.map (String >> Input state.path Select))
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]

        buildOption ( k, v ) =
            option [ value k, selected (state.value == Just k) ] [ text v ]
    in
    div
        [ class "eti-text-input__wrapper" ]
        [ selectInputLabel info state
        , select (formAttrs ++ attrs) (
            [ option [ selected True, value "" ] [ text "" ] ] --disabled True
            ++ (List.map buildOption options)
            )
        ]


checkboxInput : FieldInfo -> Input e Bool
checkboxInput info state attrs =
    let
        formAttrs =
            [ id ("radio-btn-" ++ info.id)
            , type_ "checkbox"
            , checked (state.value |> Maybe.withDefault False)
            , onCheck (Bool >> Input state.path Checkbox)
            , onFocus (Focus state.path)
            , onBlur (Blur state.path)
            ]
    in
    div [ class "eti-container"
        , style "display" "flow-root"
        ]
        [ Html.label
            [ for ("radio-btn-" ++ info.id)
            , class "erb-container"
            , style "padding" "12px"
            ]
            [ div
                [ class "erb-circle-group"
                ]
                [ div
                    [ classList
                        [ ( "erb-square-group__outer", True )
                        , ( "erb-square-group__outer--selected", Maybe.withDefault False state.value )
                        ]
                    ]
                    [ div
                        [ classList
                            [ ( "erb-square-group__inner", True )
                            ]
                        ]
                        [ span [ class "erb-checkmark" ] [] ]
                    ]
                ]
            , div
                []
                [ text (Maybe.withDefault "" info.label) ]
            ]
        , div
            [ class "erb-hidden-checkbox" ]
            [ input (formAttrs ++ attrs) [] ]
        , bottomText info state
        ]

readOnlyInput : FieldInfo -> Input e String
readOnlyInput info state attrs =
    div [ class "eti-container"
        ]
        [ div [ class "eti-text-input__wrapper" ]
            [ div
                [ class "eti-text-input__label eti-text-input__label--raised" ]
                [ span [] [ text (Maybe.withDefault "" info.label) ] ]
            , input
                [ class "eti-text-input__input"
                , readonly True
                , value (Maybe.withDefault "" state.value)
                ]
                []
            ]
        , bottomText info state
        ]

fileInput : FieldInfo -> FieldState e String -> List (Attribute Msg) -> Html Msg
fileInput info state attrs =
    div [ classList
            [ ( "eti-container", True )
            , ( "eti-outline--error", (state.error /= Nothing) )
            ]
        ]
        [ div [ class "eti-text-input__wrapper" ]
            [ div
                [ class "eti-text-input__label eti-text-input__label--raised" ]
                [ span [] [ text (Maybe.withDefault "" info.label) ] ]
            , div
                [ class "file is-small is-white eti-text-input__input level"]
                [ label [ class "file-label" ]
                    [ input
                        [ type_ "file"
                        , class "file-input"
                        , on "change" (Json.map (GotFile state.path) fileDecoder)
                        ]
                        []
                    , span
                        [ class "file-cta" ]
                        [ span [ class "file-icon" ] [ i [ class "fas fa-upload" ] [] ]
                        , span [ class "file-label" ] [ text "Upload" ]
                        ]
                    , case (Maybe.withDefault "" state.value) of
                        "" ->
                            span [ class "file-name" ] [ text "No file uploaded" ]
                        url ->
                            a [ href url, target "_blank" ] [ span [ class "file-name" ] [ text "Link to file" ] ]
                    ]
                ]
            ]
        , bottomText info state
            |> Html.map StateMsg
        ]

fileDecoder : Json.Decoder File
fileDecoder =
    Json.at [ "target", "files" ] (Json.index 0 File.decoder)
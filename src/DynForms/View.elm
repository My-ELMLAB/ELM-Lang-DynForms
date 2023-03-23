module DynForms.View exposing (viewForm, viewField, errorDecl, getFormValue)

{-| Functions used to display form elements.


# View functions

@docs viewForm

-}

import Dict
import DynForms exposing (..)
import DynForms.Field exposing (..)
import DynForms.State exposing (..)
import DynForms.Widget as Widget
import DynForms.Widget.Theme as WidgetTheme
import Form
import Form.Field exposing (FieldValue(..))
import Html exposing (Attribute, Html, div, form, text)
import Html.Attributes exposing (action, class, classList, name, step, type_)
import Maybe exposing (withDefault)


{-| Render form as Elm HTML
-}
viewForm : Form -> Html Msg
viewForm form =
    case form.layout of
        LinearLayout ids ->
            viewLinearLayout ids form

        TableLayout ids ->
            Html.table [] [viewTableLayout ids form]


errorDecl : FieldInfo
errorDecl =
    hiddenField ""



--- LAYOUT --------------------------------------------------------------


viewLinearLayout : List String -> Form -> Html Msg
viewLinearLayout ids form =
    let
        info =
            List.map (\id -> Dict.get id form.fields |> withDefault errorDecl) ids

        values =
            List.map (\id -> getFormValue id form) ids
    in
    Html.form [ class "form", action form.action ] <|
        List.map2 (viewField form) info values


viewTableLayout : List String -> Form -> Html Msg
viewTableLayout ids form =
    let
        info =
            List.map (\id -> Dict.get id form.fields |> withDefault errorDecl) ids

        values =
            List.map (\id -> getFormValue id form) ids
    in
    Html.tr []
        <| List.map (\html -> Html.td [] [html])
        <| List.map2 (viewField form) info values


viewField : Form -> FieldInfo -> FieldValue -> Html Msg
viewField form info data =
    let
        label =
            case info.label of
                Just st ->
                    [ Html.label [] [ text st ] ]

                Nothing ->
                    []

        helpText =
            case info.help of
                Just st ->
                    [ Html.br [] []
                    , Html.span [ class "form-help-text" ] [ text st ]
                    ]

                Nothing ->
                    []
    in
    getInputElement form info data


getInputElement : Form -> FieldInfo -> FieldValue -> Html Msg
getInputElement form info st =
    let
        fromText attrs =
            Form.getFieldAsString info.id form.state
                |> (\fieldst -> (name info.id :: attrs)
                    |> ( case info.fieldType of
                        DropdownField ->
                            Widget.dropdownInput (Maybe.withDefault [] info.options) info fieldst
                            
                        TextAreaField ->
                            Widget.textAreaInput info fieldst
                            
                        _ ->
                            Widget.textInput info fieldst
                        )
                    |> ( case info.fieldType of
                        TextAreaField -> WidgetTheme.wrapperLarge info fieldst
                        _ -> WidgetTheme.wrapper info fieldst
                        )
                    )
                |> Html.map StateMsg
                
        fromDate attrs =
            Form.getFieldAsString info.id form.state
                |> (\fieldst ->
                        div
                            [ classList
                                [ ( "eti-container", True )
                                , ( "eti-outline", True ) --Outline style
                                , ( "eti-outline--focused", fieldst.hasFocus )
                                , ( "eti-outline--error", (fieldst.error /= Nothing) )
                                ]
                            ]
                            [ ( name info.id :: attrs )
                                |> Widget.dateInput form info fieldst
                            , WidgetTheme.bottomLineWrapper fieldst --Filled style
                                |> Html.map StateMsg
                            , WidgetTheme.bottomText info fieldst
                                |> Html.map StateMsg
                            ]
                    )
                
        fromBool attrs =
            Form.getFieldAsBool info.id form.state
                |> (\fieldst -> (name info.id :: attrs)
                    |> Widget.checkboxInput info fieldst
                    )
                |> Html.map StateMsg
        
        fromInfo attrs =
            Form.getFieldAsString info.id form.state
                |> (\fieldst -> (name info.id :: attrs)
                    |> Widget.readOnlyInput info fieldst
                    )
                |> Html.map StateMsg
        
        fromFile attrs =
            Form.getFieldAsString info.id form.state
                |> (\fieldst -> (name info.id :: attrs)
                    |> Widget.fileInput info fieldst
                    )

    in
    case info.fieldType of
        IntField ->
            fromText [ type_ "number", step "1" ]

        FloatField ->
            fromText [ type_ "number" ]

        BoolField ->
            fromBool []
        
        DateField ->
            fromDate [ type_ "button" ]
            
        StringField ->
            fromText []

        ReadOnlyField ->
            fromInfo []

        InfoOnlyField ->
            fromInfo []
        
        FileField ->
            fromFile []

        HiddenField ->
            div [] []

        BreakField ->
            div [] []

        _ ->
            fromText []


getFormValue : String -> Form -> FieldValue
getFormValue id form =
    (Form.getFieldAsString id form.state).value
        |> Maybe.map (\st -> String st)
        |> withDefault EmptyField

module DynForms.JsonEncode
    exposing
        ( encodeData
        , encodeDatum
        , encodeFieldInfo
        , encodeLayout
        , encodeString
        , encodeValue
        , fieldTypeName
        )

{-| Functions that encode a form or parts of the form to JSON.

The most useful functions are the encodeValue that encodes the complete form
and encodeValueData that only encodes the current state of form data.

@docs encodeValue, encodeString


# Other partial encoders

Partial encoders do not implement the convenience encode-to-string function.

@docs encodeFieldInfo, encodeLayout

-}

import Dict exposing (Dict)
import DynForms exposing (..)
import DynForms.Field exposing (..)
import DynForms.Data exposing (..)
import DynForms.State exposing (..)
import Form
import Json.Encode as Enc exposing (..)
import Maybe


{-| Encode a form to a JSON value.
-}
encodeValue : Form -> Value
encodeValue form =
    let
        ids =
            Dict.keys form.fields

        fields =
            Dict.values form.fields
    in
    object
        [ ( "action", string form.action )
        , ( "fields", encodeFieldInfoList ids fields )
        , ( "data", encodeData ids fields form.state )
        , ( "layout", encodeLayout form.layout )
        ]


{-| Encode a form to a JSON string.

The first parameter is an integer value describing the indentation level. (It
can be safely set to 0.)

-}
encodeString : Int -> Form -> String
encodeString indent form =
    encode indent (encodeValue form)


encodeFieldInfoList : List String -> List FieldInfo -> Value
encodeFieldInfoList ids fields =
    let
        encodePair id field =
            ( id, encodeFieldInfo field )
    in
    object <| List.map2 encodePair ids fields


{-| Encodes a FieldInfo object to a value
-}
encodeFieldInfo : FieldInfo -> Value
encodeFieldInfo field =
    let
        type_ =
            [ ( "type", string (fieldTypeName field.fieldType) ) ]

        label =
            case field.label of
                Just st ->
                    [ ( "label", string st ) ]

                Nothing ->
                    []

        placeholder =
            case field.placeholder of
                Just st ->
                    [ ( "placeholder", string st ) ]

                Nothing ->
                    []

        help =
            case field.help of
                Just st ->
                    [ ( "help", string st ) ]

                Nothing ->
                    []

        validators =
            [ ( "validators", list string [] ) ]
            
        options =
            case field.options of
                Just st ->
                    [
                    ( "options"
                    , list (\(k,v) -> object [ ("value", string k), ("label", string v) ]) st
                    )
                    ]
                
                Nothing ->
                    []
                    
        default =
            case field.default of
                Just st ->
                    [
                    ( "default"
                    , case st of
                        String x -> string x
                        Bool x -> bool x
                        Int x -> int x
                        Float x -> float x
                        Empty -> null
                    )
                    ]
                
                Nothing ->
                    []
    in
    object <| type_ ++ label ++ placeholder ++ help ++ validators ++ options ++ default


fieldTypeName : FieldType -> String
fieldTypeName tt =
    case tt of
        BreakField ->
            "break"

        HiddenField ->
            "hidden"

        StringField ->
            "textinput"

        TextAreaField ->
            "textarea"

        FloatField ->
            "float"

        IntField ->
            "integer"

        BoolField ->
            "boolean"

        DropdownField ->
            "dropdown"

        DateField ->
            "date"

        FileField ->
            "file"

        ReadOnlyField ->
            "text"

        InfoOnlyField ->
            "info"
        
        CustomField ->
            "custom"


encodeData : List String -> List FieldInfo -> FormState -> Value
encodeData ids fields state =
    let
        encodeItem id field =
            ( id, encodeDatum id field state )
    in
    object <| List.map2 encodeItem ids fields


encodeDatum : String -> FieldInfo -> FormState -> Value
encodeDatum id info state =
    case info.dataType of
        StringDataType ->
            (Form.getFieldAsString id state).value
                |> Maybe.map string
                |> Maybe.withDefault (string "")

        IntDataType ->
            (Form.getFieldAsString id state).value
                |> Maybe.andThen (\x -> String.toInt x)
                |> Maybe.map int
                |> Maybe.withDefault null

        FloatDataType ->
            (Form.getFieldAsString id state).value
                |> Maybe.andThen (\x -> String.toFloat x)
                |> Maybe.map float
                |> Maybe.withDefault null

        BoolDataType ->
            (Form.getFieldAsBool id state).value
                |> Maybe.map bool
                |> Maybe.withDefault (bool False)


{-| Encodes a Layout object to a value
-}
encodeLayout : FormLayout -> Value
encodeLayout layout =
    case layout of
        LinearLayout ids ->
            object
                [ ( "type", string "linear" )
                , ( "fields", list string ids )
                ]

        TableLayout ids ->
            object
                [ ( "type", string "table" )
                , ( "fields", list string ids )
                ]

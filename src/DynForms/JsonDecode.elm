module DynForms.JsonDecode
    exposing
        ( decodeString
        , decodeValue
        , fieldInfoDecoder
        , fieldInfoFactory
        , fieldTypeDecoder
        , fieldDataDecoder
        , formDecoder
        , layoutDecoder
        , optionsDecoder
        , validatorDecoder
        )

{-| Functions used to decode a JSON string or Value to a form object.


# Decode functions

@docs decodeValue, decodeString


# Decoders

@docs formDecoder, layoutDecoder, fieldInfoDecoder, validatorDecoder

-}

import DynForms exposing (..)
import DynForms.Data exposing (..)
import DynForms.Field exposing (..)
import DynForms.State exposing (..)
import DynForms.Util as Util
import Json.Decode as Dec exposing (..)
import Json.Decode.Pipeline as D exposing (..)
import Json.Encode as Enc exposing (null)
import Dict exposing (Dict)


{-| Decode a JSON value to its corresponding form object
-}
decodeValue : Value -> Result Error Form
decodeValue value =
    Dec.decodeValue formDecoder value


{-| Decode a JSON string to its corresponding form object
-}
decodeString : String -> Result Error Form
decodeString st =
    Dec.decodeString formDecoder st



--- DECODERS -------------------------------------------------------------------


{-| Decodes JSON to a form object
-}
formDecoder : Decoder Form
formDecoder =
    let
        formFromData action info layout state =
            let
                updateFields x =
                    { x
                        | layout = layout
                        , state = initialState info
                    }
            in
            createForm action info
                |> updateFields
    in
    map4 formFromData
        (field "action" string)
        (field "fields" fieldListDecoder)
        (field "layout" layoutDecoder)
        (field "data" stateDecoder)


fieldListDecoder : Decoder (List FieldInfo)
fieldListDecoder =
    let
        transferId ( id, info ) =
            { info | id = id }
    in
    keyValuePairs fieldInfoDecoder
        |> andThen (\lst -> succeed (List.map transferId lst))


{-| Decodes JSON to a layout object
-}
layoutDecoder : Decoder FormLayout
layoutDecoder =
    let
        layoutBuilder tt fields =
            case tt of
                "table" ->
                    Ok (TableLayout fields)

                "linear" ->
                    Ok (LinearLayout fields)

                _ ->
                    Err ("Invalid layout: " ++ tt)
    in
    map2 layoutBuilder
        (field "type" string)
        (field "fields" (list string))
        |> andThen
            (\x ->
                case x of
                    Ok value ->
                        succeed value

                    Err msg ->
                        fail msg
            )


stateDecoder : Decoder (List ( String, FieldData ))
stateDecoder =
    let
        decodeValuePair : ( String, Value ) -> ( String, FieldData )
        decodeValuePair ( name, v ) =
            let
                data =
                    String "decoded"
            in
            ( name, data )
    in
    keyValuePairs value
        |> andThen (\x -> succeed (List.map decodeValuePair x))


optionsDecoder : Decoder ( List ( String, String ) )
optionsDecoder =
    let
        optionsBuilder k v =
            (k, v)
    in
    map2 optionsBuilder
        (field "value" string)
        (field "label" string)
        |> list



--- VALIDATORS -----------------------------------------------------------------


{-| Decodes JSON to a (validator, errorString) tuple
-}
validatorDecoder : Decoder ( Validator, String )
validatorDecoder =
    let
        single : Decoder a -> List Value -> Result Error a
        single decoder lst =
            case lst of
                [] ->
                    Err (Failure "Validator is missing an argument" Enc.null)

                n :: [] ->
                    Dec.decodeValue decoder n

                _ ->
                    Err (Failure "Validator took too many arguments" Enc.null)

        decodeFromExpr : List Value -> String -> Decoder ( Validator, String )
        decodeFromExpr tail head =
            let
                ( name, colon, error_ ) =
                    Util.partition ":" head

                error =
                    String.trimLeft error_
            in
            case name of
                "max-value" ->
                    validateValidator
                        MaxValue
                        error
                        (single float tail)

                "min-value" ->
                    validateValidator
                        MinValue
                        error
                        (single float tail)

                "max-length" ->
                    validateValidator
                        MaxLength
                        error
                        (single int tail)

                "min-length" ->
                    validateValidator
                        MinLength
                        error
                        (single int tail)

                _ ->
                    fail <| "Invalid validator: " ++ name

        decodeFromList x =
            case x of
                [] ->
                    fail "Empty validator"

                head :: tail ->
                    string |> andThen (decodeFromExpr tail)
    in
    list value |> andThen decodeFromList


validateValidator : (a -> Validator) -> String -> Result Error a -> Decoder ( Validator, String )
validateValidator constructor err arg =
    case arg of
        Ok x ->
            let
                validator =
                    constructor x
            in
            case err of
                "" ->
                    succeed ( validator, "error: " ++ Debug.toString validator )

                _ ->
                    succeed ( validator, err )

        Err msg ->
            fail ( Dec.errorToString msg )



--- VALIDATORS -----------------------------------------------------------------


{-| Decodes JSON to a FieldInfo object.
-}
fieldInfoDecoder : Decoder FieldInfo
fieldInfoDecoder =
    Dec.succeed fieldInfoFactory
        |> D.optional "id" string ""
        |> D.required "type" fieldTypeDecoder
        |> D.optional "label" (nullable string) Nothing
        |> D.optional "placeholder" (nullable string) Nothing
        |> D.optional "help" (nullable string) Nothing
        |> D.optional "default" (nullable fieldDataDecoder) Nothing
        |> D.optional "validators" (list validatorDecoder) []
        |> D.optional "options" (nullable optionsDecoder) Nothing
        |> D.optional "optional" bool False
        |> D.optional "extra" (dict string) Dict.empty


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    string
        |> andThen
            (\st ->
                case st of
                    "textinput" ->
                        succeed StringField

                    "textarea" ->
                        succeed TextAreaField

                    "integer" ->
                        succeed IntField

                    "float" ->
                        succeed FloatField

                    "boolean" ->
                        succeed BoolField

                    "break" ->
                        succeed BreakField

                    "hidden" ->
                        succeed HiddenField
                        
                    "dropdown" ->
                        succeed DropdownField
                        
                    "date" ->
                        succeed DateField
                    
                    "file" ->
                        succeed FileField
                    
                    "text" ->
                        succeed ReadOnlyField

                    "info" ->
                        succeed InfoOnlyField

                    _ ->
                        succeed CustomField

                    --_ ->
                    --    fail ("Invalid field type: " ++ st)
            )


fieldDataDecoder : Decoder FieldData
fieldDataDecoder =
    oneOf
        [ string |> andThen (\x -> succeed (String x))
        , float |> andThen (\x -> succeed (Float x))
        , int |> andThen (\x -> succeed (Int x))
        , bool |> andThen (\x -> succeed (Bool x))
        ]


fieldInfoFactory :
    String
    -> FieldType
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe FieldData
    -> List ( Validator, String )
    -> Maybe ( List ( String, String ) )
    -> Bool
    -> Dict String String
    -> FieldInfo
fieldInfoFactory id type_ label placeholder help default validators options optional extra =
    let
        dataType =
            case type_ of
                BoolField ->
                    BoolDataType

                IntField ->
                    IntDataType

                FloatField ->
                    FloatDataType

                _ ->
                    StringDataType

        base =
            hiddenField id
    in
    { base
        | fieldType = type_
        , dataType = dataType
        , label = label
        , placeholder = placeholder
        , help = help
        , default = default
        , validators = validators
        , options = options
        , optional = optional
        , extra = extra
    }

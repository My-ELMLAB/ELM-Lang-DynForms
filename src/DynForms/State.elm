module DynForms.State
    exposing
        ( FormData
        , FormErrors
        , FormState
        , FormValidation
        , initialState
        , validationFromFields
        , validationFromDictFields
        )

{-| Types and functions in this modules declares how form data is represented
and accessed internally.

This is a private module.

-}

import DynForms.Field exposing (FieldInfo, FieldType(..))
import DynForms.Data exposing (..)
import Form
import Form.Field as FormField
import Form.Validate as Validate exposing (Validation)
import Dict exposing (Dict)


type alias FormData =
    List ( String, FieldData )


{-| How form errors are represented internally
-}
type alias FormErrors =
    List String


{-| Represents data inside a form
-}
type alias FormState =
    Form.Form FormErrors FormData


type alias FormValidation =
    Validation FormErrors FormData


{-| Creates a validation object from form data
-}
validationFromFields : List FieldInfo -> Validation FormErrors FormData
validationFromFields fields =
    List.map validationFromField fields
        |> Validate.sequence


{-| Creates a validation object from form data
-}
validationFromDictFields : Dict String FieldInfo -> Validation FormErrors FormData
validationFromDictFields fields =
    validationFromFields ( Dict.values fields )


validationFromField : FieldInfo -> Validation FormErrors ( String, FieldData )
validationFromField info =
    let
        validate validator =
            Validate.field info.id ( pair validator )
        pair validator =
            Validate.map (\v -> ( info.id, v )) validator
    in
    if ( info.optional == True
        || List.member info.fieldType [BreakField, HiddenField, InfoOnlyField] ) then
        case info.dataType of
            StringDataType ->
                pair validateString

            FloatDataType ->
                pair validateFloat

            IntDataType ->
                pair validateInt

            BoolDataType ->
                pair validateBool
    else
        case info.dataType of
            StringDataType ->
                validate validateString

            FloatDataType ->
                validate validateFloat

            IntDataType ->
                validate validateInt

            BoolDataType ->
                validate validateBool


validateString : Validation FormErrors FieldData
validateString =
    Validate.customValidation Validate.string (\x -> Ok (String x))


validateInt : Validation FormErrors FieldData
validateInt =
    Validate.customValidation Validate.int (\x -> Ok (Int x))


validateFloat : Validation FormErrors FieldData
validateFloat =
    Validate.customValidation Validate.float (\x -> Ok (Float x))


validateBool : Validation FormErrors FieldData
validateBool =
    Validate.customValidation Validate.bool (\x -> Ok (Bool x))



--- STATE CONSTRUCTORS ---------------------------------------------------------


initialStateValues : List FieldInfo -> List ( String, FormField.Field )
initialStateValues lst =
    let
        initial ( dataType, default ) =
            case dataType of
                StringDataType ->
                    FormField.string (asString (Maybe.withDefault (Empty) default))

                _ ->
                    case default of
                        Nothing ->
                            FormField.value FormField.EmptyField
                        
                        _ ->
                            FormField.string (asString (Maybe.withDefault (Empty) default))
    in
    List.map (\info -> ( info.id, initial ( info.dataType, info.default ) )) lst


initialState : List FieldInfo -> FormState
initialState lst =
    let
        values =
            initialStateValues lst

        validators =
            validationFromFields lst
    in
    Form.initial values validators
module DynForms
    exposing
        ( Form
        , FormLayout(..)
        , Msg(..)
        , createForm
        , getBoolAt
        , getFieldLabelAt
        , getFieldValidatorsAt
        , getFloatAt
        , getIntAt
        , getStringAt
        , tryFieldDefaultAt
        , tryFieldHelpAt
        , tryFieldInfoAt
        , tryFieldPlaceholderAt
        , tryFieldTypeAt
        , updateForm
        )

{-| Dynamic forms usually start as some JSON sent by the backend. Elm takes control
to present to render the form, interact with the user and perform basic
validation. Data can be serialized and sent back to the server for further
validation and persistence.

If you want to generate a form from JSON, take a look at the
DynForms.JsonDecode and DynForms.JsonEncode modules. You can also construct
form objects in Elm. A form is defined by a series of fields and a layout.

    import DynForms exposing (createForm)
    import DynForms.Field exposing (..)
    import DynForms.Validation exposing (..)

    sampleForm : Form
    sampleForm =
        createForm "/api/form/1"
            [ stringField "name"
                |> label "Name"
                |> placeholder "Full name"
                |> validators [ maxLength 200 ]
            , intField "age"
                |> label "Age"
                |> help "Your age"
                |> validators [ minValue 0, maxValue 120 ]
            ]


# Form creation

@docs Form, createForm


# Basic data types

The DynForms module also defines a few auxiliary data types.

@docs FormLayout, Validator


# Field constructors and introspection

A field is declared using functions in the DynForms.Fields module. This module
defines a few functions used to construct a field (e.g., label, help,
placeholder, etc). If you want to instrospect an specific field use one of the
functions bellow.

The `try*` functions return a Maybe while the `get*` functions return a definite
value.

@docs tryFieldInfoAt, getFieldLabelAt, tryFieldDefaultAt, tryFieldPlaceholderAt
@docs tryFieldHelpAt, tryFieldTypeAt, getFieldValidatorsAt


# Data access

The content of a field can be retrieved by a few data access functions.
Notice that those are not type safe functions and you should make sure that you
always use the right function to access each field.

@docs getStringAt, getBoolAt, getFloatAt, getIntAt


# Validators

Each field may define a list of validators that defines additional validation
routines that are not simply based on the content type (set up a minimum
value of a number, or the maximum length of a string, etc). Field validators
are declared in the DynForms.Validation module.


# View Functions

View functions are declared in the DynForms.View module. Unless you are trying
someting fancy, you really should care only about the viewForm() function.


# Elm Archtecture

DynForms follows a very standard Elm archtecture. `Form` can be safely used
inside a model. It also defines the view function on the `DynForms.View`
module. You should also use `DynForms.Msg` and `DynForms.updateForm` functions.

@docs Msg, updateForm

You may want to check the self-contained example at
<https://github.com/fabiommendes/elm-dynamic-forms/blob/master/example/Example.elm>

-}

import Dict exposing (Dict)
import DynForms.Field exposing (FieldInfo, FieldType(..), Validator(..))
import DynForms.Data exposing (..)
import DynForms.State exposing (..)
import DynForms.Widget.DatePicker as DatePicker exposing (defaultSettings)
import Form
import Form.Field as FormField
import Maybe exposing (withDefault)

import Date exposing (Date)
import Time exposing (Month(..))
import File exposing (File)


--- BASIC DATA TYPES


{-| Basic form type.

The Form type represents a full form in the application including the filled in
values, declarations of field types and behaviors and the form layout.

Forms can be safely included in a model. It replicates a standard Elm
archtecture and provides standard functions and messages such as updateForm,
viewForm and FormMsg.

You should create new forms using the DynForms.form constructor.

-}
type alias Form =
    { action : String
    , fields : Dict String FieldInfo
    , layout : FormLayout
    , state : FormState
    , datePicker : DatePicker.DatePicker
    }


{-| Represents a layout for the form elements.

The Layout describes how form elements will be placed on the page when creating
a view. Check the `DynForms.Layout` module for more options.

-}
type FormLayout
    = LinearLayout (List String)
    | TableLayout (List String)


{-| The message type.
-}
type Msg
    = StateMsg Form.Msg
    | ToDatePicker DatePicker.Msg
    | GotFile String File


{-| Alias string for clarity
-}
type alias Id =
    String



--- CONSTRUCTORS ---------------------------------------------------------------


{-| Creates a new form.

You should pass the address to the endpoint that handles the form and a list of
fields.

-}
createForm : String -> List FieldInfo -> Form
createForm action info =
    { action = action
    , fields =
        Dict.fromList <|
            List.map (\x -> ( x.id, x )) info
    , layout =
        LinearLayout <|
            List.map (\x -> x.id) info
    , state =
        initialState <|
            info
    , datePicker = DatePicker.initFromDate (Date.fromCalendarDate 1992 May 31)
    }



--- FIELD INFO DATA ACCESSORS --------------------------------------------------


{-| Retrive information from a field if it exists
-}
tryFieldInfoAt : Id -> Form -> Maybe FieldInfo
tryFieldInfoAt id form =
    Dict.get id form.fields


{-| Get field type for field with the given id, if it exists.
-}
tryFieldTypeAt : Id -> Form -> Maybe FieldType
tryFieldTypeAt id form =
    tryFieldInfoAt id form
        |> Maybe.map (\info -> info.fieldType)


{-| Get field label for field with the given id, if it exists and label is
defined.
-}
getFieldLabelAt : Id -> Form -> Maybe String
getFieldLabelAt id form =
    tryFieldInfoAt id form
        |> Maybe.andThen (\info -> info.label)


{-| Get field placeholder for field with the given id, if it exists and
placeholder is defined.
-}
tryFieldPlaceholderAt : Id -> Form -> Maybe String
tryFieldPlaceholderAt id form =
    tryFieldInfoAt id form
        |> Maybe.andThen (\info -> info.placeholder)


{-| Get field default value for field with the given id, if it exists and
default value is defined.
-}
tryFieldDefaultAt : Id -> Form -> Maybe FieldData
tryFieldDefaultAt id form =
    tryFieldInfoAt id form
        |> Maybe.andThen (\info -> info.default)


{-| Get field help string for field with the given id, if it exists and the
help string is defined.
-}
tryFieldHelpAt : Id -> Form -> Maybe String
tryFieldHelpAt id form =
    tryFieldInfoAt id form
        |> Maybe.andThen (\info -> info.help)


{-| Get list of validators for the given field. If field does not exist,
return an empty list.
-}
getFieldValidatorsAt : Id -> Form -> List ( Validator, String )
getFieldValidatorsAt id form =
    tryFieldInfoAt id form
        |> Maybe.map (\info -> info.validators)
        |> Maybe.withDefault []



--- DATA ACCESSORS -------------------------------------------------------------


{-| Returns the string stored in the field with the given id of a form object.

Invalid data (e.g., non-string values) is silently converted to string or
coerced to an empty string.

    getStringAt "name" form == "R2D2"

-}
getStringAt : Id -> Form -> String
getStringAt id form =
    let
        info =
            tryFieldInfoAt id form
    in
    (Form.getFieldAsString id form.state).value |> withDefault ""


{-| Returns the float value stored in the field with the given id of a form
object.

Invalid data is silently coerced to 0.0.

-}
getFloatAt : Id -> Form -> Float
getFloatAt id form =
    getStringAt id form
        |> String.toFloat
        |> Maybe.withDefault 0


{-| Returns the integer value stored in the field with the given id of a form
object.

Invalid data is silently coerced to 0.

-}
getIntAt : Id -> Form -> Int
getIntAt id form =
    getFloatAt id form
        |> truncate


{-| Returns the boolean value stored in the field with the given id of a form
object.

Invalid data is silently coerced to False.

-}
getBoolAt : Id -> Form -> Bool
getBoolAt id form =
    let
        info =
            tryFieldInfoAt id form
    in
    (Form.getFieldAsBool id form.state).value |> withDefault False



--- TEA FUNCTIONS --------------------------------------------------------------


{-| Update form using message.
-}
updateForm : Msg -> Form -> Form
updateForm msg form =
    case msg of
        StateMsg stateMsg ->
            case stateMsg of
                Form.NoOp ->
                    form

                _ ->
                    let
                        validations =
                            validationFromFields ( Dict.values form.fields )
                    in
                    { form
                        | state = Form.update validations stateMsg form.state
                    }
        
        ToDatePicker subMsg ->
            let
                newDatePicker =
                    DatePicker.update subMsg form.datePicker
            in
            { form
                | datePicker = newDatePicker
            }
        
        GotFile id file ->
            let
                validations =
                    validationFromFields ( Dict.values form.fields )
            in
            { form
                | state = Form.update
                    validations
                    ( Form.Input id Form.Text (FormField.String <| File.name file) )
                    form.state
            }
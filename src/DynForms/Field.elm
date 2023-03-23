module DynForms.Field
    exposing
        ( boolField
        , customField
        , default
        , dateField
        , dropdownField
        , fileField
        , floatField
        , help
        , hiddenField
        , intField
        , label
        , options
        , optional
        , placeholder
        , readonlyField
        , stringField
        , textField
        , validators
        , FieldInfo
        , FieldType(..)
        , Validator(..)
        )

{-|


# Field types

@docs stringField, textField, boolField, hiddenField, intField, floatField


## Constructor helpers

The functions bellow create modified versions of the field setting up one of
its parameters. They are useful for initializing the field object using the
pipeline notation as in

    myField =
        intField "field-id"
            |> label "Number"
            |> placeholder "42"
            |> default "0"

@docs label, placeholder, help, default, validators

-}

import DynForms.Data exposing (FieldData, FieldDataType(..))
import Dict exposing (Dict)


--- FIELD TYPES --------------------------------------------------------------

{-| Stores basic information about a field.

Use the functions on `DynForms.Field` to create FieldInfo objets unless you
know what your are doing.

-}
type alias FieldInfo =
    { id : String
    , fieldType : FieldType
    , dataType : FieldDataType
    , label : Maybe String
    , placeholder : Maybe String
    , default : Maybe FieldData
    , help : Maybe String
    , validators : List ( Validator, String )
    , options : Maybe ( List ( String, String ) )
    , optional : Bool
    , extra : Dict String String
    }


{-| Field type.

Enumerate the possible field types.

-}
type FieldType
    = BreakField
    | HiddenField
    | StringField
    | TextAreaField
    | IntField
    | FloatField
    | BoolField
    | DropdownField
    | DateField
    | FileField
    | InfoOnlyField
    | ReadOnlyField
    | CustomField


{-| Validator

Describe the possible validation routines. Error strings can be set separately
from the validator. See `DynForms.Validation` for more options.

-}
type Validator
    = MinValue Float
    | MaxValue Float
    | MinLength Int
    | MaxLength Int


defaultFieldInfo : FieldInfo
defaultFieldInfo =
    { id = ""
    , fieldType = HiddenField
    , dataType = StringDataType
    , label = Nothing
    , placeholder = Nothing
    , default = Nothing
    , help = Nothing
    , validators = []
    , options = Nothing
    , optional = False
    , extra = Dict.empty
    }



--- BASIC FIELD CONSTRUCTORS ---------------------------------------------------


{-| String fields represents a simple line of string input.
-}
stringField : String -> FieldInfo
stringField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = StringField
    }


{-| Text fields are similar to regular string fields, but uses a textarea as
the default input element.
-}
textField : String -> FieldInfo
textField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = TextAreaField
    }


{-| Read only fields cannot be edited by user but is submitted together with the form
-}
readonlyField : String -> FieldInfo
readonlyField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = ReadOnlyField
    }


{-| Hidden fields hold a string value that is hidden from the user.
-}
hiddenField : String -> FieldInfo
hiddenField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = HiddenField
    }


{-| Int fields hold numeric data as whole numbers.
-}
intField : String -> FieldInfo
intField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = IntField
        , dataType = IntDataType
    }


{-| Float fields hold numeric data that can have decimal places.
-}
floatField : String -> FieldInfo
floatField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = FloatField
        , dataType = FloatDataType
    }


{-| Boolean fields hold a boolean value and are usually represented by a
checkbox.
-}
boolField : String -> FieldInfo
boolField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = BoolField
        , dataType = BoolDataType
    }
    
    
{-| Dropdown fields hold a string value selected from a list of options
-}
dropdownField : String -> FieldInfo
dropdownField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = DropdownField
    }


{-| Date fields hold an ISO date value selected using a date picker
-}
dateField : String -> FieldInfo
dateField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = DateField
    }


{-| File fields allows uploading files and holds the url of the uploaded file
-}
fileField : String -> FieldInfo
fileField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = FileField
    }

{-| Custom fields allow custom widgets to be used in place of a standard string field
-}
customField : String -> FieldInfo
customField id =
    { defaultFieldInfo
        | id = id
        , label = Just id
        , fieldType = CustomField
    }


--- CONSTRUCTOR HELPERS --------------------------------------------------------


type alias FieldMod m =
    m -> FieldInfo -> FieldInfo


{-| Sets the label text of a field.

The label is usually place just above or to the left of a field input box
to tell the user what kind of information should be filled in.

-}
label : FieldMod String
label st info =
    { info | label = Just st }


{-| Sets the placeholder text of a field.

The placeholder is a text that is displayed inside the input field and is
replaced by the user input as soon as the user starts typing. A common design
choice is to replace the label with a placeholder. DynForms treats both
independently.

-}
placeholder : FieldMod String
placeholder st info =
    { info | placeholder = Just st }


{-| Sets the default value of a field.

This pre-fills the field with some value. If this option is enabled, the
placeholder should not appear unless the user deletes the field content.

-}
default : FieldMod FieldData
default value info =
    { info | default = Just value }


{-| Defines a help string that is usually displayed bellow the field input
element.

Help strings are useful UI elements that can be used to provide additional
information to the user on how to fill the field or any other type of
clarification.

-}
help : FieldMod String
help st info =
    { info | help = Just st }


{-| Defines a list of validators for a field declaration.

Each validator is declared using the functions in the DynForms.Validators
module.

-}
validators : List (FieldInfo -> ( Validator, String )) -> FieldInfo -> FieldInfo
validators validator_list info =
    let
        extra : List ( Validator, String )
        extra =
            List.map (\f -> f info) validator_list
    in
    { info | validators = info.validators ++ extra }


{-| Defines a list of options for a field declaration.

Each option is a key-value pair of Strings.

-}
options : FieldMod ( List ( String, String ) )
options option_list info =
    { info | options = Just option_list }

{-| Make field optional.

This allows the field to be empty. If this option is enabled,
validation error will not be shown for this field.

-}
optional : FieldMod Bool
optional flag info =
    { info | optional = flag }
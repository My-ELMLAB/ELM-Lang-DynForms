module DynamicForms exposing (..)

import DynForms exposing (..)
import DynForms.Field exposing (..)
import DynForms.Data exposing (FieldData(..))
import DynForms.JsonDecode exposing (..)
import DynForms.JsonEncode exposing (..)
import DynForms.Validation exposing (..)
import DynForms.View exposing (..)
import Html exposing (Html, code, div, h1, h2, p, pre, text)
import Browser

import Task
import Date exposing (Date)
import DynForms.Widget.DatePicker as DatePicker


-- Declares a form


sampleForm : Form
sampleForm =
    createForm "/api/form/1"
        [ stringField "name"
            |> label "Name"
            --|> placeholder "Full name"
            |> help "Your name and family name in the order you'd like to be called."
            |> validators [ maxLength 20 ]
            |> default (String "Bobby")
        , textField "comments"
            |> label "Comments"
            |> help "Write some angry stuff here."
            |> validators [ maxLength 200 ]
            |> default (String "This form sucks")
            |> optional True
        , intField "age"
            |> label "Age"
            |> help "Your age"
            |> validators [ minValue 0, maxValue 120 ]
            |> default (Int 10)
        , floatField "cash"
            |> label "Cash"
            |> help "Input some cash"
            |> validators [ minValue 0, maxValue 120 ]
            |> default (Float 10.50)
        , boolField "happy"
            |> label "I'm happy"
            |> help "Are you happy?"
            |> default (Bool True)
        , dropdownField "poison"
            |> label "Choose your poison"
            |> help "Choose whatever you like"
            |> options
                [ ("apple", "Apple")
                , ("boy", "Boy")
                , ("cat", "Cat")
                , ("dog", "Dog")
                , ("epoxy", "Epoxy")
                ]
        , dateField "from"
            |> label "From"
            |> help "From when?"
            |> default (String "2017-07-07")
        , dateField "to"
            |> label "To"
            |> help "To when?"
        , fileField "upload"
            |> label "Receipt"
            |> help "Where is your receipt?"
        , readonlyField "total"
            |> label "Total"
            |> help "How much?"
            |> default (Int 10)
        , customField "custom"
            |> label "Custom Field"
            |> help "Test custom field should work like a string field"
        ]



-- Setup form validation


init : String -> ( Form, Cmd Msg )
init flags =
    ( sampleForm
    , Task.perform
        ( \d -> ToDatePicker (DatePicker.CurrentDate d) )
        Date.today
    )



-- Render form with Input helpers


view : Form -> Html Msg
view form =
    let
        json =
            encodeString 4 form

        rebuiltForm =
            decodeString json

        fromJsonForm =
            case rebuiltForm of
                Ok newform ->
                    viewForm newform

                Err msg ->
                    p [] [ text (Debug.toString msg) ]
    in
    div []
        [ h1 [] [ text "Dynamic forms example" ]
        , h2 [] [ text "Here is the form" ]
        , viewForm form
        , h2 [] [ text "Its representation as a JSON string" ]
        , code []
            [ pre [] [ text json ]
            ]
        , h2 [] [ text "Form reconstructed from JSON string input" ]
        , fromJsonForm
        ]


main =
    Browser.element
        { init = init
        , update = \msg f -> ( updateForm msg f, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }

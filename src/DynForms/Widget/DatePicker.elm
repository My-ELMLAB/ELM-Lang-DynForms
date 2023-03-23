module DynForms.Widget.DatePicker exposing
    ( Msg(..), DatePicker(..), Settings, defaultSettings
    , init, initFromDate, initFromDates, update, focusedDate
    , getInitialDate, between, moreOrLess, from, to, off
    )


import Date exposing (Date, Month, day, month, year)
import DynForms.Widget.Date exposing (..)
import Html exposing (..)
import Html.Attributes as Attrs exposing (placeholder, selected, tabindex, type_, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, targetValue)
import Html.Keyed
import Json.Decode as Json
import Task
import Time exposing (Weekday(..))


{-| An opaque type representing messages that are passed inside the DatePicker.
-}
type Msg
    = CurrentDate Date
    | ChangeFocus Date


{-| The type of date picker settings.
-}
type alias Settings =
    { isDisabled : Date -> Bool
    , dateFormatter : Date -> String
    , firstDayOfWeek : Weekday
    , changeYear : YearRange
    }


type alias Model =
    { focused : Maybe Date -- date currently center-focused by picker, but not necessarily chosen
    , today : Date -- actual, current day as far as we know
    }


{-| The DatePicker model. Opaque, hence no field docs.
-}
type DatePicker
    = DatePicker Model


{-| A record of default settings for the date picker. Extend this if
you want to customize your date picker.

To disable certain dates:

    import Date exposing (Day(..), dayOfWeek)
    import DatePicker exposing (defaultSettings)

    DatePicker.init { defaultSettings | isDisabled = \d -> dayOfWeek d `List.member` [ Sat, Sun ] }

-}
defaultSettings : Settings
defaultSettings =
    { isDisabled = always False
    , dateFormatter = Date.format "dd/MM/yyyy"
    , firstDayOfWeek = Sun
    , changeYear = off
    }


yearRangeActive : YearRange -> Bool
yearRangeActive yearRange =
    yearRange /= Off


{-| Select a range of date to display

    DatePicker.init { defaultSettings | changeYear = between 1555 2018 }

-}
between : Int -> Int -> YearRange
between start end =
    if start > end then
        Between end start

    else
        Between start end


{-| Select a symmetric range of date to display

    DatePicker.init { defaultSettings | changeYear = moreOrLess 10 }

-}
moreOrLess : Int -> YearRange
moreOrLess range =
    MoreOrLess range


{-| Select a range from a given year to this year

    DatePicker.init { defaultSettings | changeYear = from 1995 }

-}
from : Int -> YearRange
from year =
    From year


{-| Select a range from this year to a given year

    DatePicker.init { defaultSettings | changeYear = to 2020 }

-}
to : Int -> YearRange
to year =
    To year


{-| Turn off the date range

    DatePicker.init { defaultSettings | changeYear = off }

-}
off : YearRange
off =
    Off


{-| The default initial state of the Datepicker. You must execute
the returned command (which, for the curious, sets the current date)
for the date picker to behave correctly.

    init =
        let
            ( datePicker, datePickerFx ) =
                DatePicker.init
        in
        ( { picker = datePicker }, Cmd.map ToDatePicker datePickerfx )

-}
init : ( DatePicker, Cmd Msg )
init =
    ( DatePicker <|
        { focused = Just initDate
        , today = initDate
        }
    , Task.perform CurrentDate Date.today
    )


{-| Initialize a DatePicker with a given Date

    init date =
        ( { picker = DatePicker.initFromDate date }, Cmd.none )

-}
initFromDate : Date -> DatePicker
initFromDate date =
    DatePicker <|
        { focused = Just date
        , today = date
        }


{-| Initialize a DatePicker with a date for today and Maybe a date picked

    init today date =
        ( { picker = DatePicker.initFromDates today date }, Cmd.none )

-}
initFromDates : Date -> Maybe Date -> DatePicker
initFromDates today date =
    DatePicker <|
        { focused = date
        , today = today
        }


prepareDates : Date -> Weekday -> { currentMonth : Date, currentDates : List Date }
prepareDates date firstDayOfWeek =
    let
        weekdayAsInterval =
            weekdayToInterval firstDayOfWeek

        firstOfMonth =
            Date.fromCalendarDate (year date) (month date) 1

        -- First shown date
        -- If the first of a month is a sunday and firstDayOfWeek is sunday then its the first of the month
        -- Otherwise the daterange starts in the month before the current month
        start =
            Date.fromCalendarDate (year date) (month date) 1
                |> Date.floor weekdayAsInterval

        end =
            Date.add Date.Months 1 firstOfMonth
                |> Date.ceiling weekdayAsInterval
    in
    { currentMonth = date
    , currentDates = Date.range Date.Day 1 start end
    }


{-| Expose the currently focused date
-}
focusedDate : DatePicker -> Maybe Date
focusedDate (DatePicker model) =
    model.focused

{-| Expose the initial date

When you initialize the DatePicker using function `init` the resulting `Cmd Msg` fetches todays date.
This date is then stored in the DatePicker's model as initial date.

In some scenarios, you want use todays date in combination with `DatePicker.Settings`.
This allows you to use todays date without storing it yourself.

Check the `simple-with-validate-date` example for an example usage.

-}
getInitialDate : DatePicker -> Date
getInitialDate (DatePicker model) =
    model.today


{-| The date picker update function. The second tuple member represents a user action to change the
date.
-}
update : Msg -> DatePicker -> DatePicker
update msg (DatePicker ({ focused } as model)) =
    case msg of
        CurrentDate date ->
            DatePicker { model | focused = Just date, today = date }

        ChangeFocus date ->
            DatePicker { model | focused = Just date }
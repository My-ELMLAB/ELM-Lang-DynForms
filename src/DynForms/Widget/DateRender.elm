module DynForms.Widget.DateRender exposing
    ( view
    , strToDate
    )
    
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Form exposing (InputType(..), Msg(..), FieldState)
import Form.Field as Field exposing (FieldValue(..))
import Form.Input exposing (Input)
import Json.Decode as Json

import Date exposing (Date, Month, day, month, year)
import DynForms.Widget.DatePicker as DatePicker exposing (DatePicker(..), Settings, Msg(..))
import DynForms.Widget.Date exposing (..)
import DynForms exposing (Msg(..))
import Time exposing (Weekday(..), Month(..))


{-| The date picker view. The Date passed is whatever date it should treat as selected.
-}
view : Settings -> DatePicker -> FieldState e String -> List (Attribute Msg) -> Html Msg
view settings model state attrs =
    let
        ( today, focused ) =
            case model of
                DatePicker m -> (m.today, m.focused)

        initialDate = -- if 1st arg else 2nd arg else 3rd arg
            (strToDate state.value) |> maybeOr focused |> Maybe.withDefault today
    
        inputClasses =
            [ ( styleClass "input", True )
            , ( "eti-text-input__input", True )
            , ( "_datepicker_textinput__input", True )
            ]

        inputCommon xs =
            input
                ([ Attrs.classList inputClasses
                 , type_ "text"
                 --, onBlur (Form.Blur state.path)
                 , onClick (Form.Focus state.path) |> Attrs.map StateMsg
                 , onFocus (DatePicker.ChangeFocus (initialDate)) |> Attrs.map ToDatePicker
                 ]
                    ++ List.map (\attrib -> Attrs.map (\x -> StateMsg Form.NoOp) attrib) attrs
                    ++ List.map (\attrib -> Attrs.map StateMsg attrib) xs
                )
                []

        dateInput =
            inputCommon
                [ ( Maybe.map settings.dateFormatter (strToDate state.value)
                    |> Maybe.withDefault ""
                    )
                |> value
                ]

        containerClassList =
            [ ( styleClass "container", True ) ]
    in
    div
        [ Attrs.classList containerClassList ]
        [ dateInput
        , if state.hasFocus then
            datePicker settings model state
          else
            text ""
        , if state.hasFocus then
            div [class (styleClass "unfocus"), onClick (Form.Blur state.path)] []
                |> Html.map StateMsg
          else
            text ""
        ]
        

datePicker : Settings -> DatePicker -> FieldState e String -> Html Msg
datePicker settings (DatePicker ({ today, focused } as model)) state =
    let
        currentDate =
            focused |> Maybe.withDefault today

        { currentMonth, currentDates } =
            prepareDates currentDate settings.firstDayOfWeek

        dpClass a =
            Attrs.class (styleClass a)

        firstDayOffset =
            Date.weekdayToNumber settings.firstDayOfWeek - 1

        arrow className message =
            button
                [ dpClass className
                , onClick message
                , tabindex -1
                , type_ "button"
                ]
                []

        isToday d =
            Date.toRataDie d == Date.toRataDie today

        isOtherMonth d =
            month currentDate /= month d

        dayList =
            groupDates currentDates
                |> List.map
                    (\rowDays ->
                        tr [ dpClass "row" ]
                            (List.map (viewDay settings isOtherMonth isToday state) rowDays)
                    )

        onChange handler =
            on "change" <| Json.map handler targetValue

        isCurrentYear selectedYear =
            year currentMonth == selectedYear

        yearOption index selectedYear =
            ( String.fromInt index
            , option [ value (String.fromInt selectedYear), selected (isCurrentYear selectedYear) ]
                [ text <| String.fromInt selectedYear ]
            )

        dropdownYear =
            Html.Keyed.node "select"
                [ --onChange (changeYear currentDate >> ChangeFocus),
                 dpClass "year-menu" ]
                (List.indexedMap yearOption
                    (yearRange { currentMonth = currentMonth, today = today } settings.changeYear)
                )
    in
    div
        [ dpClass "picker" ]
        [ div [ dpClass "picker-header" ]
            [ div [ dpClass "prev-container" ]
                [ arrow "prev" (DatePicker.ChangeFocus (Date.add Date.Months -1 currentDate))
                ] |> Html.map ToDatePicker
            , div [ dpClass "month-container" ]
                [ span [ dpClass "month" ]
                    [ text <| formatMonth <| month currentMonth ]
                , span [ dpClass "year" ]
                    [ if not (yearRangeActive settings.changeYear) then
                        text <| String.fromInt <| year currentMonth

                      else
                        Html.Keyed.node "span" [] [ ( String.fromInt (year currentMonth), dropdownYear ) ]
                    ]
                ]
            , div [ dpClass "next-container" ]
                [ arrow "next" (DatePicker.ChangeFocus (Date.add Date.Months 1 currentDate))
                ] |> Html.map ToDatePicker
            ]
        , table [ dpClass "table" ]
            [ thead [ dpClass "weekdays" ]
                [ tr []
                    ([ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]
                        |> List.repeat 2
                        |> List.concat
                        |> List.drop firstDayOffset
                        |> List.take 7
                        |> List.map (\d -> td [ dpClass "dow" ] [ text <| formatDay d ])
                    )
                ]
            , tbody [ dpClass "days" ] dayList
            ]
        ]


viewDay : Settings -> (Date -> Bool) -> (Date -> Bool)
    -> FieldState e String -> Date -> Html Msg
viewDay settings isOtherMonth isToday state d =
    let
        disabled =
            ( settings.isDisabled d || isOtherMonth d )
            
        picked =
            (strToDate state.value)
                |> Maybe.map (\pdate -> Date.toRataDie pdate == Date.toRataDie d)
                |> Maybe.withDefault False

        props =
            if not disabled then
                [ onClick <| Input state.path Form.Text (String (Date.toIsoString d)) ]

            else
                []
    in
    td
        ([ classList
            [ ( styleClass "day", True )
            , ( styleClass "disabled", disabled )
            , ( styleClass "picked", picked && not disabled )
            , ( styleClass "today", isToday d )
            , ( styleClass "other-month", isOtherMonth d )
            ]
         ]
            ++ props
        )
        [ text <| String.fromInt <| Date.day d ]
    |> Html.map StateMsg


strToDate : Maybe String -> Maybe Date
strToDate str =
    case str of
        Just value ->
            let
                result = Date.fromIsoString value
            in
            case result of
                Ok date -> Just date
                _ -> Nothing
        _ -> Nothing


yearRangeActive : YearRange -> Bool
yearRangeActive yearRange =
    yearRange /= Off


{-| Turn a list of dates into a list of date rows with 7 columns per
row representing each day of the week.
-}
groupDates : List Date -> List (List Date)
groupDates dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xxs ->
                    if i == 6 then
                        go 0 xxs [] (List.reverse (x :: racc) :: acc)

                    else
                        go (i + 1) xxs (x :: racc) acc
    in
    go 0 dates [] []
    
        
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


styleClass : String -> String
styleClass c =
    ("elm-datepicker--" ++ c)


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr lhs rhs =
    case rhs of
        Just _ ->
            rhs

        Nothing ->
            lhs
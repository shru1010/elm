module Calculator exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)
import Browser exposing(..)
main : Program () Model Msg
main=
    Browser.sandbox{init=init, update=update,view=view}

--model
--this type alias has all the functions i want to use
type alias Calculator =
    { add : Float->Float->Float
    , sub : Float->Float->Float
    , mult : Float->Float->Float
    , div : Float->Float->Float
    }

--creating func called calc which is following type Calc
--im defining all the functions
calculator : Calculator
calculator =
    { add = (\x y -> x + y)
    , sub = (\x y -> x - y)
    , mult = (\x y -> x * y)
    , div = (\x y -> x / y)
    }

--creating out actual model,displaying string,functions input and output,
--save value is for just storing previous value so that when we use operator x value doesnt vanish
--append is to keep adding digits or decimals to a value.
type alias Model =
    { 
        display:String,
        function:Float->Float->Float,
        saveValue:Float,
        append:Bool

    }
--init

init:Model
init=
    {
        display="",
        function=(\x y->y),
        saveValue=0,
        append=True
    }

--taking string and parsing it to a float
parseFloat:String->Float
parseFloat input=
    Maybe.withDefault 0 (String.toFloat input)


operation:Model->(Float->Float->Float)->Model
operation model function=
    {
        model|function=function
        ,saveValue=(parseFloat model.display)
        ,append=False
    }

--Msg
type Msg
    = None
    | Add
    | Sub
    | Mult
    | Div
    | Equal
    | Decimal
    | Zero
    | Number Int
    | Clear


--update
update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model

        Clear ->
            init

        Number number ->
            updateDisplay model number
        
        Decimal ->
            decimal model

        Zero->
            zero model
        Add ->
            operation model calculator.add
        Sub ->
            operation model calculator.sub
        Mult ->
            operation model calculator.mult
        Div ->
            operation model calculator.div
        Equal->
            equal model
--func is used to input values for calculation
updateDisplay:Model ->Int -> Model
updateDisplay model number=
    if model.append then   --if append is true then concat the new number to the previous number
        {
            model|display=model.display ++ Debug.toString(number)
        }
    else{model|display=Debug.toString(number),append=True}
    --if append is false then convert the new digit to a string and change append to true so that new digits can be added to it

equal : Model->Model
equal model=
    if model.append then
    {
        model|display=calculate model, saveValue=(parseFloat model.display), append=False
    }
    else{model|display=calculate model,append=False}

calculate : Model->String
calculate model=
    model.function model.saveValue(parseFloat model.display)
    |>Debug.toString

zero : Model -> Model
zero model =
    if String.isEmpty model.display || not model.append then
        { model
            | display = "0"
            , append = False
        }
    else
        { model | display = model.display ++ "0" }

decimal : Model -> Model
decimal model =
    if not (String.isEmpty model.display) && model.append then
        { model | display = appendDecimal model.display }
    else
        { model | display = "0.", append = True }


appendDecimal : String -> String
appendDecimal string =
    if String.contains "." string then
        string
    else
        string ++ "."



-- VIEW


calculatorButton : Msg -> String -> Html Msg
calculatorButton msg buttonText =
    button [ onClick msg ]
        [ span [] [ text (buttonText) ] ]


calculatorButtonWide : Msg -> String -> Html Msg
calculatorButtonWide msg buttonText =
    button [ onClick msg ]
        [ span [] [ text (buttonText) ] ]




view : Model -> Html Msg
view model =
    div [ ]
        [  div [ ]
                [ text (model.display) ]
                ,div[][
                    calculatorButton (Number 7) "7"
                    , calculatorButton (Number 8) "8"
                    , calculatorButton (Number 9) "9",
                    calculatorButton Div "÷"],
                    div[][
                     calculatorButton (Number 4) "4"
                    , calculatorButton (Number 5) "5"
                    , calculatorButton (Number 6) "6"
                    , calculatorButton Mult "x"] ,
                    div[][calculatorButton (Number 1) "1"
                    , calculatorButton (Number 2) "2"
                    , calculatorButton (Number 3) "3"
                    , calculatorButton Sub "-"] ,
                    div[][
                     calculatorButton Zero "0"
                    , calculatorButton Decimal "."
                    , calculatorButton Equal "="
                    , calculatorButton Add "+"
                    ],
                    div[][calculatorButtonWide Clear "Clear"]
                 
        ]

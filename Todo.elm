module Todo exposing (..)
import Browser exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
--import Html.App as App


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


type alias Model =
    { todo : String
    , todos : List String
    }


initialModel : Model
initialModel =
    { todo = ""
    , todos = []
    }



--update


type Msg
    = UpdateTodo String
    | AddTodo
    | RemoveItem String
    | RemoveAll
    | ClearInput


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTodo text ->
            { model | todo = text }

        AddTodo ->
            { model | todos = model.todo :: model.todos }

        RemoveItem text ->
            { model | todos = List.filter (\x -> x /= text) model.todos }

        RemoveAll ->
            { model | todos = [] }

        ClearInput ->
            { model | todo = "" }



--view




todoItem : String -> Html Msg
todoItem todo =
    li [  ] [ text todo, button [ onClick (RemoveItem todo) ] [ text "x" ] ]


todoList : List String -> Html Msg
todoList todos =
    let
        child =
            List.map todoItem todos
    in
        ul [] child


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "text"
            , onInput UpdateTodo
            , value model.todo
            , onMouseEnter ClearInput
            ]
            []
        , button [ onClick AddTodo ] [ text "Submit" ]
        , button [ onClick RemoveAll ] [ text "Remove All" ]
        , todoList model.todos
        ]


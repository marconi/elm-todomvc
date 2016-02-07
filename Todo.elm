module Todo where

import Debug
import String exposing (toLower)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Graphics.Element exposing (show)
import StartApp.Simple as StartApp
import Signal
import Signal exposing (Address)

-- MODEL

type alias Todo =
  { id: Int
  , name: String
  , isCompleted: Bool
  }


newTodo : Int -> String -> Bool -> Todo
newTodo id name isCompleted =
  { id = id
  , name = name
  , isCompleted = isCompleted
  }


type alias Model =
  { todos: List Todo
  , leftCounter: Int
  , appliedFilter: Filter
  , completedCounter: Int
  , todoInput: String
  , nextId: Int 
  , isCompletedAll: Bool
  }

initialModel : Model
initialModel =
  { todos = []
  , leftCounter = 0
  , appliedFilter = All
  , completedCounter = 0
  , todoInput = ""
  , nextId = 1
  , isCompletedAll = False
  }

-- UPDATE

type Action
  = NoOp
  | Add
  | UpdateTodoInput String
  | ToggleCompleted Int
  | Delete Int
  | ToggleCompletedAll
  | SelectFilter Filter

update action model =
  Debug.watchSummary "update action" (\_ -> (action, model)) <|
  case action of
    NoOp ->
      model

    Add ->
      let
        todo = newTodo model.nextId model.todoInput False
      in
        { model | todos = todo :: model.todos
        , todoInput = ""
        , leftCounter = model.leftCounter + 1
        , nextId = model.nextId + 1
        }

    UpdateTodoInput input ->
      { model | todoInput = input }

    ToggleCompletedAll ->
      let
        isCompletedAll = (not model.isCompletedAll)
        todos = List.map (\todo -> { todo | isCompleted = isCompletedAll }) model.todos
        todosCounter = List.length todos
      in
        { model | isCompletedAll = isCompletedAll
        , todos = todos
        , leftCounter = if isCompletedAll then 0 else todosCounter
        , completedCounter = if isCompletedAll then todosCounter else 0
        }

    ToggleCompleted id ->
      let
        toggledTodos = List.filter (\todo -> todo.id == id) model.todos
        toggleCompleted todo =
          if todo.id == id then
            { todo | isCompleted = (not todo.isCompleted) }
          else
            todo
        isTodoCompleted = (List.length <| List.filter .isCompleted toggledTodos) > 0
        leftCounter = if isTodoCompleted then model.leftCounter + 1 else model.leftCounter - 1
        completedCounter = if isTodoCompleted then model.completedCounter - 1 else model.completedCounter + 1
      in
        { model | todos = List.map toggleCompleted model.todos
        , leftCounter = leftCounter
        , completedCounter = completedCounter
        }

    Delete id ->
      let
        nonDeletedTodos = List.filter (\todo -> todo.id /= id) model.todos
        completedCounter = List.length <| List.filter .isCompleted nonDeletedTodos
        leftCounter = (List.length nonDeletedTodos) - completedCounter
      in
        { model | todos = nonDeletedTodos
        , leftCounter = leftCounter
        , completedCounter = completedCounter
        }

    SelectFilter filter ->
      { model | appliedFilter = filter }

-- VIEW

type Filter
  = All
  | Active
  | Completed


onTodoInput address action =
  on "input" targetValue (\value -> Signal.message address <| action value)


onTodoEnter address action =
  let
    keycodeAction code = if code == 13 then Add else NoOp
  in
    on "keyup" keyCode (\code -> Signal.message address <| keycodeAction code)


getHeader address todoInput =
  header
    [ id "header" ]
    [ h1 [] [ text "todos" ]
    , input
        [ type' "text"
        , id "new-todo"
        , placeholder "What needs to be done?"
        , name "newTodo"
        , autofocus True
        , value todoInput
        , onTodoInput address UpdateTodoInput
        , onTodoEnter address Add
        ]
        []
    ]


getTodoItem : Address Action -> Todo -> Html
getTodoItem address todo =
  li
    [ classList [ ("completed", todo.isCompleted) ] ]
    [ div
        [ class "view" ]
        [ input
            [ class "toggle"
            , type' "checkbox"
            , checked todo.isCompleted
            , onClick address <| ToggleCompleted todo.id 
            ]
            []
        , label [] [ text todo.name ]
        , button
            [ class "destroy"
            , onClick address <| Delete todo.id
            ]
            []
        ]
    , input
        [ class "edit"
        , name "title"
        , id <| "todo-" ++ (toString todo.id)
        ]
        []
    ]


getTodos address appliedFilter todos =
  let
    filterTodo todo =
      if appliedFilter == Active && not todo.isCompleted then
        True
      else if appliedFilter == Completed && todo.isCompleted then
        True
      else if appliedFilter == All then
        True
      else
        False
    filteredTodos = List.filter filterTodo todos
    todoItems = List.map (getTodoItem address) filteredTodos
  in
    ul
      [ id "todo-list" ]
      todoItems


getFilters address appliedFilter =
  let
    filterItem filter =
      li
        []
        [ a
            [ href <| toLower <| "#" ++ (toString filter)
            , classList [ ("selected", appliedFilter == filter) ]
            , onClick address <| SelectFilter filter
            ]
            [ text <| toString filter ]
        ]
  in
    ul
      [ id "filters" ]
      [ filterItem All
      , filterItem Active
      , filterItem Completed
      ]


getFooter address leftCounter completedCounter appliedFilter =
  footer
    [ id "footer" ]
    [ span
        [ id "todo-count" ]
        [ strong [] [ leftCounter |> toString |> text ]
        , text " item left"
        ]
    , getFilters address appliedFilter
    , button
        [ class "clear-completed", id "clear-completed" ]
        [ text <| "Clear completed (" ++ (toString completedCounter) ++ ")" ]
    ]


getFooterInfo =
  footer
    [ id "info" ]
    [ p [] [ text "Double-click to edit a todo" ]
    , p
        []
        [ text "Written by "
        , a [ href "https://github.com/marconi" ] [ text "Marconi Moreto" ]
        ]
    , p
        []
        [ text "Part of "
        , a [ href "http://todomvc.com" ] [ text "TodoMVC" ] ]
    ]


view address model =
  div
    [ class "todomvc-wrapper" ]
    [ section
        [ id "todoapp" ]
        [ getHeader address model.todoInput
        , section
            [ id "main" ]
            [ input
                [ id "toggle-all"
                , type' "checkbox"
                , name "toggle"
                , onClick address ToggleCompletedAll
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , getTodos address model.appliedFilter model.todos
            ]
        , getFooter address model.leftCounter model.completedCounter model.appliedFilter
        ]
    , getFooterInfo
    ]


main =
  StartApp.start
    { model = initialModel
    , update = update
    , view = view
    }

module Main exposing(main)
import Browser
import Html exposing (Html, button, div, text, input, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main =
    Browser.sandbox { init = init, update = update, view = view }


-- Model
type alias Task = { title : String
                  , status : Int
                  }

type alias Model = { tasks: List Task
                   , titleInput : String
                   }
init : Model
init = Model [] ""

-- Update
type Msg
  = Add
  | Delete Int
  | Update Int String
  | ChangeTitleInput String

update : Msg -> Model -> Model
update msg model =
  case msg of
      Add ->
          { model |
            tasks = List.append model.tasks [(Task model.titleInput 0)]
          , titleInput = ""
          }
      Delete i ->
          { model | tasks = List.append (List.take i model.tasks) (List.drop (i + 1) model.tasks) }
      Update i title ->
          let target = (List.head (List.drop i model.tasks))
              targetStatus = case target of
                              Just x -> x.status
                              Nothing -> 0
          in { model |
                   tasks = List.append (List.append (List.take i model.tasks)
                                            [Task title targetStatus])
                   (List.drop (i + 1) model.tasks)
             }
      ChangeTitleInput titleInput ->
          { model | titleInput = titleInput }

-- View

view : Model -> Html Msg
view model =
    div []
        [ div []
              [ input [ type_ "text", id "todo-input", placeholder "Typing Todo..."
                      , value model.titleInput , onInput ChangeTitleInput ]
                    []
              , button [ onClick Add ] [ text "create" ]
              ]
        , div [] (List.map
                      (\x -> a [ onClick (Delete x.status) ]
                           [
                            div []
                                [ text ("状態: " ++ (String.fromInt x.status))
                                , text " =>  "
                                , text ("title: " ++ x.title)
                                ]
                           ]
                      )
                      model.tasks)
        ]

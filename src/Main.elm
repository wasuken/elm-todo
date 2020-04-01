module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as CBlock
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size
import Bootstrap.Badge as Badge

import Browser
import Html exposing (Html, a, button, div, h3, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias Task =
    { id : Int
    , title : String
    , status : Int
    }


type alias Model =
    { tasks : List Task
    , titleInput : String
    , updateTaskTitleInput : String
    , updatingTask : Task
    , mode : String
    }


init : Model
init =
    Model [] "" "" (Task -1 "" 0) "normal"



-- Function


updateTask : List Task -> Int -> String -> Int -> List Task
updateTask tasks id title status =
    List.map
        (\x ->
            case x.id == id of
                True ->
                    Task id title status

                False ->
                    x
        )
        tasks


deleteTask : Int -> List Task -> List Task
deleteTask targetId tasks =
    let
        taskTail =
            case List.tail tasks of
                Just tks ->
                    tks

                Nothing ->
                    []
    in
    (case List.head tasks of
        Just task ->
            case task.id == targetId of
                True ->
                    []

                False ->
                    [ task ]

        Nothing ->
            []
    )
        ++ (case taskTail of
                [] ->
                    []

                _ ->
                    deleteTask targetId taskTail
           )


taskRender : Task -> Model -> Html Msg
taskRender task model =
    let
        titleInputOrText =
            case model.mode == "update" && model.updatingTask.id == task.id of
                True ->
                    p []
                        [ Input.text
                            [ Input.placeholder "Typing Todo..."
                            , Input.value model.updateTaskTitleInput
                            , Input.onInput ChangeUpdatingTaskTitle
                            , Input.attrs [ Size.w50, Display.inlineBlock ]
                            ]
                        , Button.button
                            [ Button.primary
                            , Button.attrs [ onClick UpdatedTaskTitle ]
                            ]
                            [ text "Commit" ]
                        ]

                False ->
                    text task.title
        badge = case task.status of
                     0 -> Badge.badgePrimary [ ] [ text "todo" ]
                     1 -> Badge.badgeSecondary [ ] [ text "doing" ]
                     2 -> Badge.badgeSuccess [ ] [ text "doit" ]
                     _ -> Badge.badgeDanger [ ] [ text "??????" ]
        buttons =
            [ Button.button [ Button.primary, Button.attrs [ onClick (Update task.id task.title 0)] ]
                [ text "Todo"
                ]
            , Button.button [ Button.primary, Button.attrs [ onClick (Update task.id task.title 1) ] ]
                [ text "Doing"
                ]
            , Button.button [ Button.primary, Button.attrs [ onClick (Update task.id task.title 2) ] ]
                [ text "Doit"
                ]
            , Button.button [ Button.primary, Button.attrs [ onClick (UpdatingTaskTitle task) ] ]
                [ text "UpdatingTitle"
                ]
            , Button.button [ Button.danger, Button.attrs [ onClick (Delete task.id) ] ]
                [ text "×"
                ]
            ]
    in
    div []
        [ Card.config [ Card.attrs [ Size.w50 ] ]
            |> Card.header
                []
                [ h3 [] [ badge, titleInputOrText ] ]
            |> Card.block
                []
                [ CBlock.titleH4 [] buttons
                ]
            |> Card.view

        -- text ("状態: " ++ String.fromInt task.status)
        --  , text " =>  "
        --  , titleInputOrText
        ]



-- Update


type Msg
    = Add
    | Delete Int
    | Update Int String Int
    | ChangeTitleInput String
    | UpdatingTaskTitle Task
    | UpdatedTaskTitle
    | ChangeUpdatingTaskTitle String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            let
                maxIdPlusOne =
                    case List.maximum (List.map (\x -> x.id) model.tasks) of
                        Just x ->
                            x + 1

                        Nothing ->
                            0
            in
            { model
                | tasks = List.append model.tasks [ Task maxIdPlusOne model.titleInput 0 ]
                , titleInput = ""
            }

        ChangeUpdatingTaskTitle v ->
            { model | updateTaskTitleInput = v }

        Delete id ->
            { model | tasks = deleteTask id model.tasks }

        Update id title status ->
            { model
                | tasks = updateTask model.tasks id title status
            }

        ChangeTitleInput titleInput ->
            { model | titleInput = titleInput }

        UpdatingTaskTitle task ->
            { model
                | updatingTask = task
                , updateTaskTitleInput = task.title
                , mode = "update"
            }

        UpdatedTaskTitle ->
            { model
                | mode = "normal"
                , tasks =
                    updateTask model.tasks
                        model.updatingTask.id
                        model.updateTaskTitleInput
                        model.updatingTask.status
            }



-- View


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , div []
            [ Input.text
                [ Input.id "todo-input"
                , Input.placeholder "Typing Todo..."
                , Input.value model.titleInput
                , Input.onInput ChangeTitleInput
                , Input.attrs [ Size.w50, Display.inlineBlock ]
                ]
            , button [ onClick Add ] [ text "create" ]
            ]
        , div [] (List.map (\x -> taskRender x model) model.tasks)
        ]

port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { notes : List Note
    , uid : Int
    , currentNoteId : Int
    }

emptyModel : Model
emptyModel =
    { notes = []
    , uid = 0
    , currentNoteId = 0
    }

type alias Note =
    { text : String
    , id : Int
    , timeCreated : String
    , isSelected : Bool
    }

newNote : String -> Int -> String -> Note
newNote text id time =
    { text = text
    , id = id
    , timeCreated = time
    , isSelected = False
    }

currentNote : Model -> Note
currentNote model =
    Maybe.withDefault (newNote "" 0 "") (List.head (List.filter (\n -> n.id == model.currentNoteId) model.notes))

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "scroll col col-xs-3" ]
                [ noteList model.notes ]
            , div [ class "col col-xs-9" ]
                [ noteEditor (currentNote model) ]
            ]
        ]

noteList : List Note -> Html Msg
noteList notes =
    ul [ class "note-list list-group" ]
        (List.reverse (List.map noteItem notes))

noteItem : Note -> Html Msg
noteItem note =
    let
        noteText =
            if String.isEmpty (String.trim note.text) then
                "New Note"
            else
                note.text
    in
        li [ classList [ ( "list-group-item", True ), ( "note-selected", note.isSelected ) ] ]
            [ div
                [ classList [ ( "new-note", (String.isEmpty (String.trim note.text)) ) ]
                  , Events.onClick (SelectNote note.id)
                ]
                [ span [ class "note-title" ] [ text (truncateText noteText 20) ]
                , span [ class "note-time" ] [ text note.timeCreated ]
                , button
                    [ class "btn btn-primary note-delete"
                    , Events.onClick (DeleteNote note.id)
                    ]
                    [ span [ class "glyphicon glyphicon-trash" ] []
                    ]
                ]
            ]

noteEditor : Note -> Html Msg
noteEditor note =
    div []
        [ textarea
            [ class "note-editor"
            , Events.onInput (\str -> (UpdateNote note.id str))
            , value note.text
            ]
            []
        , button
            [ classList [ ( "add-note btn btn-primary", True ), ( "btn", True ) ]
            , Events.onClick CreateNote
            ]
            [ span [ class "glyphicon glyphicon-plus" ] [] ]
        ]

truncateText : String -> Int -> String
truncateText text len =
    if String.length text > len then
        String.concat [ (String.left len text), "..." ]
    else
        text

-- UPDATE

type Msg
    = CreateNote
    | AddNote String
    | UpdateNote Int String
    | DeleteNote Int
    | SelectNote Int

port createNote : String -> Cmd msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNote ->
            ( model, createNote "a" )

        AddNote noteTimeCreated ->
            let
                newModel =
                    { model
                        | uid = model.uid + 1
                        , notes =
                            model.notes ++ [ newNote "" (model.uid + 1) noteTimeCreated ]
                        , currentNoteId = model.uid + 1
                    }

                updateSelection n =
                    { n | isSelected = n.id == newModel.currentNoteId }

                updateSelectionInModel m =
                    { m | notes = List.map updateSelection m.notes }
            in
                ( updateSelectionInModel newModel, Cmd.none )

        UpdateNote id text ->
            let
                updateNote n =
                    if n.id == id then
                        { n | text = text }
                    else
                        n
            in
                ( { model | notes = List.map updateNote model.notes }, Cmd.none )

        DeleteNote id ->
            let
                newModel =
                    { model
                        | notes = List.filter (\n -> n.id /= id) model.notes
                    }
                updateSelectionIfDeleted model =
                    if model.currentNoteId == id then
                     { model
                        | currentNoteId = 1
                     }
                    else
                        model
            in
            ( updateSelectionIfDeleted newModel, Cmd.none )

        SelectNote id ->
            let
                newModel =
                    { model
                        | currentNoteId = id
                    }

                updateSelection n =
                    { n | isSelected = n.id == newModel.currentNoteId }

                updateSelectionInModel m =
                    { m | notes = List.map updateSelection m.notes }
            in
                ( updateSelectionInModel newModel, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )

-- SUBSCRIPTIONS

port notes : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    notes AddNote

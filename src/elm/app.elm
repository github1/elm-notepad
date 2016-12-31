port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Json

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
    , currentNoteId : Int
    }

emptyModel : Model
emptyModel =
    { notes = []
    , currentNoteId = 0
    }

type alias Note =
    { text : String
    , id : Int
    , timeCreated : String
    }

type alias NoteUpdate =
    { id : Int, text : String }

createNoteUpdatePayload : Int -> String -> NoteUpdate
createNoteUpdatePayload id text =
    { id = id
    , text = text
    }

newNote : String -> Int -> String -> Note
newNote text id time =
    { text = text
    , id = id
    , timeCreated = time
    }

currentNote : Model -> Note
currentNote model =
    Maybe.withDefault (newNote "" 0 "") (List.head (List.filter (\n -> n.id == model.currentNoteId) model.notes))

-- VIEW

view : Model -> Html Msg
view model = if List.isEmpty model.notes then
        div [ class "container-fluid" ]
            [ div [ class "row" ]
                [  div [ class "col col-xs-12" ]
                   [
                        button
                            [ classList [ ( "empty-add-note btn btn-lg btn-primary", True ), ( "btn", True ) ] , Events.onClick CreateNote ]
                            [ text "Get started" ]
                   ]
                ]
            ]
    else
        div [ class "container-fluid" ]
            [ div [ class "row" ]
                [ div [ class "scroll col col-xs-4" ]
                    [ noteList model model.notes ]
                , div [ class "col col-xs-8" ]
                    [ noteEditor model (currentNote model) ]
                ]
            ]

noteList : Model -> List Note -> Html Msg
noteList model notes =
    ul [ class "note-list list-group" ]
        (List.reverse (List.map (\n -> noteItem model n ) notes))

noteItem : Model -> Note -> Html Msg
noteItem model note =
    let
        noteText =
            if String.isEmpty (String.trim note.text) then
                "New Note"
            else
                note.text
    in
        li [ classList [ ( "list-group-item", True ), ( "note-selected", (.id note == .currentNoteId model)) ] ]
            [ div
                [ classList [ ( "new-note", (String.isEmpty (String.trim note.text)) ) ]
                  , Events.onClick (SelectNote note.id)
                ]
                [ div [ class "note-title" ] [ text noteText ]
                , div [ class "note-time" ] [ text note.timeCreated ]
                , button
                    [ class "btn btn-primary note-delete"
                    , Events.onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed <| DeleteNote note.id)
                    ]
                    [ span [ class "glyphicon glyphicon-trash" ] []
                    ]
                ]
            ]

noteEditor : Model -> Note -> Html Msg
noteEditor model note =
    div []
        [ textarea
            [ class "note-editor"
            , Events.onInput (\str -> UpdateNote <| createNoteUpdatePayload note.id str)
            , value note.text
            , disabled (model.currentNoteId == 0)
            ]
            []
        , button
            [ classList [ ( "add-note btn btn-primary", True ), ( "btn", True ) ]
            , Events.onClick CreateNote
            ]
            [ span [ class "glyphicon glyphicon-plus" ] [] ]
        ]

-- UPDATE

type Msg
    = CreateNote
    | UpdateCreatedNote (List Note)
    | UpdateNote NoteUpdate
    | UpdateNotes (List Note)
    | DeleteNote Int
    | SelectNote Int

port createNote : () -> Cmd msg

port modifyNote : NoteUpdate -> Cmd msg

port deleteNote : Int -> Cmd msg

applyUpdate : Msg -> Model -> ( Model, Cmd Msg )
applyUpdate msg model =
    case msg of
        CreateNote ->
            ( model, createNote () )

        UpdateCreatedNote noteList ->
            ( { model | notes = noteList, currentNoteId = Maybe.withDefault model.currentNoteId <| List.head <| List.map (\n -> .id n) (List.reverse noteList) }, Cmd.none )

        UpdateNotes noteList ->
            ( { model | notes = noteList }, Cmd.none )

        UpdateNote noteUpdatePayload ->
            ( model, modifyNote noteUpdatePayload )

        DeleteNote id ->
            ( model, deleteNote id )

        SelectNote id ->
            ( { model | currentNoteId = id } , Cmd.none )

applySelection : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applySelection ( model, msg) =
    let
        selectionMissing mdl = List.isEmpty <| List.filter (\n -> .id n == mdl.currentNoteId) mdl.notes
        newSelection mdl = if selectionMissing mdl then
                { mdl | currentNoteId = Maybe.withDefault 0 <| List.head <| List.map (\n -> .id n) <| List.reverse mdl.notes }
            else
                mdl
    in
    ( newSelection model, msg )
    --( { model | currentNoteId = Maybe.withDefault 0 <| List.head <| List.map (\n -> .id n) <| List.filter (\n -> n.id == model.currentNoteId) model.notes }, msg )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = applySelection <| applyUpdate msg model

init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )

-- SUBSCRIPTIONS

port notes : (List Note -> msg) -> Sub msg

port noteCreated : (List Note -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        notes UpdateNotes
      , noteCreated UpdateCreatedNote
    ]

port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Json.Decode as Json exposing (..)


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
view model =
    if List.isEmpty model.notes then
        div [ class "container-fluid" ]
            [ div [ class "row" ]
                [ div [ class "col col-xs-12" ]
                    [ button
                        [ classList [ ( "empty-add-note btn btn-lg btn-primary", True ), ( "btn", True ) ], Events.onClick AddNoteCommand ]
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
        (List.reverse (List.map (\n -> noteItem model n) notes))


noteItem : Model -> Note -> Html Msg
noteItem model note =
    let
        noteText =
            if String.isEmpty (String.trim note.text) then
                "new note"
            else
                note.text
    in
        li [ classList [ ( "list-group-item", True ), ( "note-selected", (.id note == .currentNoteId model) ) ] ]
            [ div
                [ classList [ ( "new-note", (String.isEmpty (String.trim note.text)) ) ]
                , Events.onClick (SelectNoteCommand note.id)
                ]
                [ div [ class "note-title" ] [ text noteText ]
                , div [ class "note-time" ] [ text note.timeCreated ]
                , button
                    [ class "btn btn-primary note-delete"
                    , Events.onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed <| DeleteNoteCommand note.id)
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
            , Events.onInput (\str -> UpdateNoteCommand note.id str)
            , Html.Attributes.value note.text
            , disabled (model.currentNoteId == 0)
            ]
            []
        , button
            [ classList [ ( "add-note btn btn-primary", True ), ( "btn", True ) ]
            , Events.onClick AddNoteCommand
            ]
            [ span [ class "glyphicon glyphicon-plus" ] [] ]
        ]



-- UPDATE

type Msg
    = AddNoteCommand
    | DeleteNoteCommand Int
    | SelectNoteCommand Int
    | UpdateNoteCommand Int String
    | ApplyEvents (List String)

port commandPort : List String -> Cmd msg

type alias EventHeader = { eventType : String }

applyUpdate : Msg -> Model -> ( Model, Cmd Msg )
applyUpdate msg model =
    case msg of
        AddNoteCommand ->
            ( model, commandPort ["AddNoteCommand"] )

        DeleteNoteCommand id ->
            ( model, commandPort ["DeleteNoteCommand", "id", toString id] )

        SelectNoteCommand id ->
            ( model, commandPort ["SelectNoteCommand", "id", toString id] )

        UpdateNoteCommand id text ->
            ( model, commandPort ["UpdateNoteCommand", "id", toString id, "text", text] )

        ApplyEvents serializedEvents ->
            let
                decodedEvents = List.map (\s ->
                    case Json.decodeString (Json.field "@type" string) s of
                    Err msg ->
                        { eventType = "None", payload = s }
                    Ok result ->
                        { eventType = result, payload = s }) serializedEvents
                decodedEventsStr = String.join "," <| List.map (\e -> .eventType e) decodedEvents
                applyEvents model = List.foldl (\t acc ->
                        if .eventType t == "NoteAddedEvent" then
                            let
                                noteDecoder : Decoder Note
                                noteDecoder =
                                            Json.map3 Note
                                                (Json.field "text" string)
                                                (Json.field "id" int)
                                                (Json.field "timeCreated" string)
                                note = case Json.decodeString noteDecoder (.payload t) of
                                    Err msg -> newNote "" 0 ""
                                    Ok result -> result
                            in
                            { acc | notes = (::) note acc.notes }
                        else if .eventType t == "NoteDeletedEvent" then
                            let
                                idToDelete = case Json.decodeString (field "id" int) (.payload t) of
                                    Err msg -> 0
                                    Ok result -> result
                            in
                            { acc | notes = List.filter (\n -> .id n /= idToDelete) acc.notes }
                        else if .eventType t == "NoteSelectedEvent" then
                            let
                                idToSelect = case Json.decodeString (field "id" int) (.payload t) of
                                    Err msg -> 0
                                    Ok result -> result
                            in
                            { acc | currentNoteId = idToSelect }
                        else if .eventType t == "NoteUpdatedEvent" then
                            let
                                idToUpdate = case Json.decodeString (field "id" int) (.payload t) of
                                    Err msg -> 0
                                    Ok result -> result
                                textUpdate = case Json.decodeString (field "text" string) (.payload t) of
                                    Err msg -> ""
                                    Ok result -> result
                            in
                            { acc | notes = List.map (\n -> if .id n == idToUpdate then newNote textUpdate idToUpdate (.timeCreated n) else n) acc.notes }
                        else
                            acc
                    ) model decodedEvents
            in
            ( applyEvents emptyModel, if List.isEmpty decodedEvents then Cmd.none else commandPort ["PrintDecodedEvents",  decodedEventsStr] )

applySelection : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applySelection ( model, msg ) =
    let
        selectionMissing mdl =
            List.isEmpty <| List.filter (\n -> .id n == mdl.currentNoteId) mdl.notes

        newSelection mdl =
            if selectionMissing mdl then
                { mdl | currentNoteId = Maybe.withDefault 0 <| List.head <| List.map (\n -> .id n) <| List.reverse mdl.notes }
            else
                mdl
    in
        ( newSelection model, msg )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    applySelection <| applyUpdate msg model


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )


-- SUBSCRIPTIONS

port eventPort: (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    eventPort ApplyEvents


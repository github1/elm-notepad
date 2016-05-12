import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Basics exposing(toString)
import String exposing(..)
import Time exposing (Time, second)
import Date exposing (year, hour, minute, second, fromTime)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subs
    }

-- MODEL

type alias Model =
    { notes : List Note
    , uid : Int
    , currentNoteId : Int
    , currentTime : Float
    }

emptyModel : Model
emptyModel =
    { notes = []
    , uid = 0
    , currentNoteId = 0
    , currentTime = 0
    }

type alias Note =
    { text : String
    , id : Int
    , timeCreated : Float
    , isSelected : Bool
    }

newNote : String -> Int -> Float -> Note
newNote text id time =
    { text = text
    , id = id
    , timeCreated = time
    , isSelected = False
    }

currentNote : Model -> Note
currentNote model =
   Maybe.withDefault (newNote "" 0 0) (List.head (List.filter (\n -> n.id == model.currentNoteId) model.notes))

-- VIEW

view : Model -> Html Msg
view model =
    div
      [ class "container-fluid" ]
      [ div [ class "row" ]
          [
             div
              [ class "scroll col col-xs-2" ]
              [ noteList model.notes ]
          ,  div
              [ class "col col-xs-9" ]
              [ noteEditor ( currentNote model ) ]
          ]
      ]

noteList : List Note -> Html Msg
noteList notes =
    ul
      [ class "note-list list-group" ]
      (List.reverse (List.map noteItem notes))

noteItem : Note -> Html Msg
noteItem note =
    let noteText =
        if String.isEmpty (String.trim note.text) then "New Note"
        else note.text
    in
    li
      [ classList [ ("list-group-item", True), ("note-selected", note.isSelected ) ] ]
        [ div [ classList [ ("new-note", (String.isEmpty (String.trim note.text))) ] ] [
              span [ class "note-title" ] [ text (truncateText noteText 20) ]
            , span [ class "note-time" ] [ text ( formatTime note.timeCreated ) ]
            , button [
                  class "btn btn-primary note-delete"
                , Events.onClick (DeleteNote note.id)
            ] [
                span [ class "glyphicon glyphicon-trash" ] []
            ]
        ]
      ]

noteEditor : Note -> Html Msg
noteEditor note =
    div
      []
      [ textarea
            [ class "note-editor"
            , Events.onInput (\str -> (UpdateNote note.id str) )
            , value note.text ]
            []
        , button
           [ classList [ ("add-note btn btn-primary", True), ("btn", True) ]
           , Events.onClick AddNote ]
           [ span [ class "glyphicon glyphicon-plus"] [] ]
      ]

formatTime : Float -> String
formatTime t =
  let date' = fromTime t
      hour24' = Date.hour date'
      hour' = String.padLeft 2 '0' (Basics.toString (if hour24' > 12 then hour24' - 12 else hour24'))
      period' = if hour24' > 12 then "PM" else "AM"
      minute' = String.padLeft 2 '0' (Basics.toString (Date.minute date'))
      day' = String.padLeft 2 '0' (Basics.toString (Date.day date'))
      month' = String.padLeft 2 '0' (Basics.toString (Date.month date'))
      year' = Basics.toString (year date')
      now = "" ++ month' ++ " " ++ day' ++ " " ++ year' ++ " " ++ hour' ++ ":" ++ minute' ++ " " ++ period'
  in
  now

truncateText : String -> Int -> String
truncateText text len =
    if String.length text > len
    then String.concat [ (String.left len text), "..." ]
    else text

-- UPDATE

type Msg
    = Tick Time
    | AddNote
    | UpdateNote Int String
    | DeleteNote Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      let
        newModel = { model | currentTime = newTime }
      in
      (newModel, Cmd.none)
    AddNote ->
      let
        newModel = { model |
                   uid = model.uid + 1
                 , notes =
                     model.notes ++ [newNote "" (model.uid + 1) model.currentTime]
                 , currentNoteId = model.uid + 1
             }
        updateSelection n = { n | isSelected = n.id == newModel.currentNoteId }
        updateSelectionInModel m = { m |
        notes = List.map updateSelection m.notes
        }
      in
      (updateSelectionInModel newModel, Cmd.none)
    UpdateNote id text ->
      let
        updateNote n = if n.id == id then { n | text = text } else n
      in
      ({ model | notes = List.map updateNote model.notes }, Cmd.none)
    DeleteNote id ->
      ({ model | notes = List.filter (\n -> n.id /= id) model.notes }, Cmd.none)


init : (Model, Cmd Msg)
init =
  (emptyModel, Cmd.none)


subs : Model -> Sub Msg
subs model =
  Time.every Time.second Tick

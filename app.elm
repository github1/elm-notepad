import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Window

---- MODEL ----

type alias Model =
    { notes : List Note
    , uid : Int
    , currentNoteId : Int
    }

type alias Note =
    { text : String
    , id : Int
    }

newNote : String -> Int -> Note
newNote text id =
    { text = text
    , id = id
    }

emptyModel : Model
emptyModel =
    { notes = [ newNote "" 0 ]
    , uid = 0
    , currentNoteId = 0
    }

currentNote : Model -> Note
currentNote model =
   Maybe.withDefault (newNote "" 0) (List.head (List.filter (\n -> n.id == model.currentNoteId) model.notes))

---- UPDATE ----

type Action
    = NoOp
    | Add
    | Update Int String
    | Delete Int

update: Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      Add ->
          { model |
                uid = model.uid + 1
              , notes =
                  model.notes ++ [newNote "" (model.uid + 1)]
              , currentNoteId = model.uid + 1
          }

      Update id text ->
          let updateNote n = if n.id == id then { n | text = text } else n
          in
          { model | notes = List.map updateNote model.notes }

      Delete id ->
          { model | notes = List.filter (\n -> n.id /= id) model.notes }

---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    div
      [ class "container-fluid" ]
      [ div [ class "row" ]
          [ div
              [ class "scroll col col-xs-2" ]
              [ lazy2 noteList address model.notes ]
          ,  div
              [ class "col col-xs-10" ]
              [ lazy2 noteEditor address (currentNote model) ]
          ]
      ]

noteList : Address Action -> List Note -> Html
noteList address notes =
    ul
      [ class "note-list list-group" ]
      (List.reverse (List.map (noteItem address) notes))

noteItem : Address Action -> Note -> Html
noteItem address note =
    let noteText =
        if String.isEmpty note.text then "New note"
        else note.text
    in
    li
      [ class "list-group-item" ]
      [ div [ class "note-info" ] [text (toString note.id) ]
        , div [ classList [ ("new-note", (String.isEmpty note.text)) ] ] [
            text (truncateText noteText 15)
        ]
      ]

truncateText : String -> Int -> String
truncateText text len =
    if String.length text > len
    then String.concat [ (String.left len text), "..." ]
    else text

noteEditor : Address Action -> Note -> Html
noteEditor address note =
    let
        update text =
          Update note.id text
    in
    div
      []
      [
        span [ class "note-info" ] [text (toString note.id) ]
        , textarea
        [ class "note-editor"
        , on "input" targetValue (Signal.message address << update)
        , value note.text
        ]
        []
        , button [
               classList [
                  ("add-note", True),
                  ("btn", True)
               ]
               , on "click" targetValue (\_ -> Signal.message address Add)
           ]
           [ text "+" ]
      ]

onEnter : Address a -> a -> Attribute
onEnter address value =
    onWithOptions
      "keydown"
      {preventDefault = True, stopPropagation = False}
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

---- INPUTS ----
main : Signal Html
main =
  Signal.map (view actions.address) model

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

initialModel : Model
initialModel =
  emptyModel

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


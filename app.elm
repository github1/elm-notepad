import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Time exposing (every, second)
import Date exposing (year, hour, minute, second, fromTime)
import Window

---- MODEL ----

type alias Model =
    { notes : List Note
    , uid : Int
    , currentNoteId : Int
    , currentTime : Float
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

emptyModel : Model
emptyModel =
    { notes = []
    , uid = 0
    , currentNoteId = 0
    , currentTime = 0
    }

currentNote : Model -> Note
currentNote model =
   Maybe.withDefault (newNote "" 0 0) (List.head (List.filter (\n -> n.id == model.currentNoteId) model.notes))

---- UPDATE ----

type Action
    = NoOp
    | Add
    | Update Int String
    | Select Int
    | Delete Int
    | UpdateTime Float

update: Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      Add ->
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
          updateSelectionInModel newModel

      Update id text ->
          let updateNote n = if n.id == id then { n | text = text } else n
          in
          { model | notes = List.map updateNote model.notes }

      Select id ->
          let updateSelection n = { n | isSelected = n.id == id }
          in
          { model | currentNoteId = id, notes = List.map updateSelection model.notes }

      Delete id ->
          { model | notes = List.filter (\n -> n.id /= id) model.notes }

      UpdateTime time ->
          let setSelected n = { n |
                         isSelected = True
              }
          in
          if (model.uid == 0) && (List.isEmpty model.notes) then
          { model |
                    currentTime = time
                  , uid = model.uid + 1
                  , currentNoteId = model.uid + 1
                  , notes = [ setSelected (newNote "" (model.uid + 1) time) ] }
          else { model | currentTime = time }


---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    div
      [ class "container-fluid" ]
      [ div [ class "row" ]
          [ div
              [ class "scroll col col-xs-3" ]
              [ lazy2 noteList address model.notes ]
          ,  div
              [ class "col col-xs-9" ]
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
        if String.isEmpty (String.trim note.text) then "New Note"
        else note.text
    in
    li
      [ classList [ ("list-group-item", True), ("note-selected", note.isSelected ) ]
      , onClick address (Select note.id) ]
        [ div [ classList [ ("new-note", (String.isEmpty (String.trim note.text))) ] ] [
              span [ class "note-title" ] [ text (truncateText noteText 20) ]
            , span [ class "note-time" ] [ text ( formatTime note.timeCreated ) ]
            , button [
                  class "btn btn-primary note-delete"
                , onClick address (Delete note.id)
            ] [
                span [ class "glyphicon glyphicon-trash" ] []
            ]
        ]
      ]

formatTime : Float -> String
formatTime t =
  let date' = fromTime t
      hour24' = Date.hour date'
      hour' = String.padLeft 2 '0' (toString (if hour24' > 12 then hour24' - 12 else hour24'))
      period' = if hour24' > 12 then "PM" else "AM"
      minute' = String.padLeft 2 '0' (toString (Date.minute date'))
      day' = String.padLeft 2 '0' (toString (Date.day date'))
      month' = String.padLeft 2 '0' (toString (Date.month date'))
      year' = toString (year date')
      now = "" ++ month' ++ " " ++ day' ++ " " ++ year' ++ " " ++ hour' ++ ":" ++ minute' ++ " " ++ period'
  in
  now

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
      [ textarea
            [ class "note-editor"
            , on "input" targetValue (Signal.message address << update)
            , value note.text ]
            []
        , button
           [ classList [ ("add-note btn btn-primary", True), ("btn", True) ]
           , on "click" targetValue (\_ -> Signal.message address Add) ]
           [ span [ class "glyphicon glyphicon-plus"] [] ]
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

timeSignal : Signal Action
timeSignal =
    Signal.map (\t -> ( UpdateTime t )) (Time.every Time.millisecond)

model : Signal Model
model =
  Signal.foldp update initialModel (Signal.merge actions.signal timeSignal)

initialModel : Model
initialModel =
  emptyModel

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


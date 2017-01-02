import moment from 'moment';

var noteId = 0;
var events = [];

const app = Elm.Main.fullscreen();

app.ports.commandPort.subscribe((arr) => {
    if (arr.length > 0) {
        var type = arr[0];
        var transaction = [];
        switch (type) {
            case "AddNoteCommand":
                var id = ++noteId;
                transaction.push({
                    "@type": "NoteAddedEvent",
                    id: id,
                    text: "",
                    timeCreated: moment().format("MMM Do YYYY, h:mm:ss a")
                });
                transaction.push({
                    "@type": "NoteSelectedEvent",
                    id: id
                });
                break;
            case "DeleteNoteCommand":
                transaction.push({
                    "@type": "NoteDeletedEvent",
                    id: parseInt(arr[2])
                });
                break;
            case "SelectNoteCommand":
                transaction.push({
                    "@type": "NoteSelectedEvent",
                    id: parseInt(arr[2])
                });
                break;
            case "UpdateNoteCommand":
                transaction.push({
                    "@type": "NoteUpdatedEvent",
                    id: parseInt(arr[2]),
                    text: arr[4]
                });
                break;
            case "PrintDecodedEvents":
                var eventNames = arr[1].split(",");
                switch (eventNames[eventNames.length - 1]) {
                    case "NoteSelectedEvent":
                        setTimeout(() => document.getElementsByClassName("note-editor")[0].focus(), 50);
                        break;
                }
                break;
        }
        if (transaction.length > 0) {
            events = events.concat(transaction);
            app.ports.eventPort.send(events.map(event => JSON.stringify(event)));
        }
    }
});

/*-
 app.ports.createNote.subscribe(() => {
 var note = {
 id: ++noteId,
 text: "",
 timeCreated: moment().format('MMMM Do YYYY, h:mm:ss a')
 };
 notes.push(note);
 app.ports.noteCreated.send(notes);
 setTimeout(() => {
 document.getElementsByClassName("note-editor")[0].focus();
 }, 100);
 });
 app.ports.modifyNote.subscribe(noteUpdate => {
 notes.forEach(note => {
 if(note.id === noteUpdate.id) {
 note.text = noteUpdate.text;
 }
 });
 app.ports.notes.send(notes);
 });
 app.ports.deleteNote.subscribe(id => {
 notes = notes.filter(note => note.id !== id);
 app.ports.notes.send(notes);
 });
 */
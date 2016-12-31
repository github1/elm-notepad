import moment from 'moment';

var noteId = 0;
var notes = [];
const app = Elm.Main.fullscreen();
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
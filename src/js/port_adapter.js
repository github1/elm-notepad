import moment from 'moment';
const app = Elm.Main.fullscreen();
var noteId = 0;
app.ports.createNote.subscribe(function() {
    app.ports.notes.send(moment().format('MMMM Do YYYY, h:mm:ss a'));
});
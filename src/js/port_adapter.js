import moment from 'moment';

const app = Elm.Main.fullscreen();

var noteId = 0;
var events = [];

const toObj = (arr) => {
    if (arr.length === 0) {
        return {type: "undefined"};
    }
    var obj = {
        type: arr[0]
    };
    for (var i = 1, il = arr.length; i < il; i++) {
        var type = arr[i++].split(":");
        var value = arr[i];
        switch (type[1] || "string") {
            case "number":
                value = parseFloat(value);
                break;
            default:
                value = "" + value;
                break;
        }
        obj[type[0]] = value;
    }
    return obj;
};

app.ports.commandPort.subscribe((arr) => {
    var command = toObj(arr);
    var transaction = [];
    switch (command.type) {
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
                id: command.id
            });
            break;
        case "SelectNoteCommand":
            transaction.push({
                "@type": "NoteSelectedEvent",
                id: command.id
            });
            break;
        case "UpdateNoteCommand":
            transaction.push({
                "@type": "NoteUpdatedEvent",
                id: command.id,
                text: command.text
            });
            break;
        case "PrintDecodedEvents":
            var eventNames = command.eventNames.split(",");
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
});

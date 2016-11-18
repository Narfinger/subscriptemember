var socket = new WebSocket("ws://localhost:8000/");

socket.onOpen = function(event) {
    console.log("Open done");
    socket.onMessage = function(event) {
	console.log(event);
    };
    socket.send("Here's some text that the server is urgently awaiting!");
};
socket.onclose = function(event) { console.log(event.code); console.log(event); };
socket.onerror = function(event) { console.log(event); };

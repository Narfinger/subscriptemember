var socket = new WebSocket("ws://localhost:8000/socketserver");

socket.onOpen = function(event) {
    console.log("Open done");
    socket.onMessage = function(event) {
	console.log(event);
    };
    socket.send("Here's some text that the server is urgently awaiting!");
};
socket.onError = function(event) {
	console.log(event);
};

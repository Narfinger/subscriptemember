var socket = new WebSocket("ws://localhost:8000/socketserver", "protocolOne");
socket.onMessage = function(event) {
    console.log(event);
}

var socket = new WebSocket("ws://localhost:8000/");

socket.onerror = function(event) { console.log("onerror"); console.log(event); };
socket.onclose = function(event) { console.log("onclose"); console.log(event.code); console.log(event); };
socket.onopen = function(event) {
    console.log("Open done");
    socket.onmessage = function(event) {
	console.log("onmessage");
	console.log(event.data);
    };
};

updateVids = function() {
    console.log("update running");
    var http = new XMLHttpRequest();
    http.open("GET","/upvids", true);
    http.send();
};

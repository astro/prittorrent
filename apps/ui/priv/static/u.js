function restoreLogin() {
    $('#login').slideDown(500);
}

$('#login').submit(function(ev) {
    $('#login').slideUp(100);

    var ws = new WebSocket("ws://" + document.location.host + "/u", "prittorrent-ui-1");
    var send = function(obj) {
	var data = JSON.stringify(obj);
	ws.send(data);
    };

    ws.onopen = function() {
	send({ user: $('#user').val(),
	       password: $('#password').val()
	     });
	var app = new Application(send);
	ws.onmessage = function() {
	    console.log("onmessage", arguments);
	};
    };
    ws.onclose = restoreLogin;
    ws.onerror = restoreLogin;

    return false;
});

function Application(send) {
    this.send = send;
}

Application.prototype = {};
Application.prototype.recv = function() {
};

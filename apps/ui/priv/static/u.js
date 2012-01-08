function restoreLogin() {
    $('.u').hide();
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
	ws.onmessage = function(message) {
	    var msg = JSON.parse(message.data);
	    app.recv(msg);
	};
    };
    ws.onclose = restoreLogin;
    ws.onerror = restoreLogin;

    return false;
});

function Application(send) {
    this.send = send;
    this.userFeeds = new UserFeedsView(send);
    $('.u').fadeIn(500);
}

Application.prototype = {};
Application.prototype.recv = function(msg) {
    if (msg.feeds)
	this.userFeeds.setFeeds(msg.feeds);
};

function UserFeedsView(send) {
    this.send = send;
    this.feeds = [];
    this.currentFeed = null;
    this.feedView = null;
    this.ul = $('#feedlist');
    this.render();

    var that = this;
    $('#addfeed').click(function() {
	that.currentFeed = null;
	delete that.feedView;
	/* Cause AddFeedView to be created */
	that.render();
    });
}

UserFeedsView.prototype = {};
UserFeedsView.prototype.render = function() {
    var that = this;
    this.ul.empty();
    this.feeds.forEach(function(feed) {
	var li = $('<li></li>');
	li.text(feed);
	if (that.currentFeed === feed)
	    li.addClass('selected');
	li.click(function() {
	    that.setCurrentFeed(feed);
	});
	that.ul.append(li);
    });

    /* Do we need to create a feedView? */
    if (!this.feedView) {
	if (this.currentFeed) {
	} else {
	    /* No currentFeed, display addfeed dialog */
	    this.feedView = new AddFeedView(this, this.send);
	}
    }
};
UserFeedsView.prototype.setFeeds = function(feeds) {
    this.feeds = feeds;
    this.render();
};

function AddFeedView(userfeeds, send) {
    $('#addfeed').show();
    $('#feeddetails').hide();

    $('#addfeedform').submit(function(e) {
	e.preventDefault();

	var input = $('#addfeedurl');
	console.log("#addfeedurl", input, input.val())
	send({ addFeed: input.val() });
	setTimeout(function() {
	    input.val("");
	}, 10);
    });
}

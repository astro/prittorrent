var username = $('#username');
var hint = $('.signup .hint');
function fixUsername() {
    var s = username.val().
	toLowerCase().
	replace(/[^0-9a-z\-_]/g, "");

    if (s !== username.val())
	username.val(s);

    if (s.length > 0)
	hint.text("http://bitlove.org/" + s);
    else
	hint.text("");
}
username.bind('change', fixUsername);
username.bind('keyup', fixUsername);


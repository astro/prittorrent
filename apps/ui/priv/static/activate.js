$('#activate').click(function(ev) {
    var activate = $('#activate');
    activate.hide();
    ev.preventDefault();

    var token = activate.attr('data-token');
    var salt = activate.attr('data-salt');
    var password1 = $('#password1').val();
    var password2 = $('#password2').val();

    function progress(s) {
	$('#progress').text(s);
    }
    function fail(s) {
	progress(s);
	activate.show();
	return;
    }

    /* Validate form data */
    if (password1 != password2)
	return fail("Passwords must match");

    /* Calculate salted password */
    progress("Salting password");
    var salted = hmac(salt, password1);

    /* Request salt+challenge for hashing */
    progress("Requesting account activation...");
    $.ajax({ type: 'POST',
	     data: {
		 salted: salted
	     },
	     url: '/activate/' + token,
	     success: function(response) {
		 if (response && response.welcome) {
		     progress("Welcome to Bitlove!");
		     document.location = response.welcome;
		 } else {
		     fail((response && response.error) || "Cannot activate");
		 }
	     },
	     error: function() {
		 fail("Error sending request");
	     }
    });
});

function hmac(key, text) {
    var hmac = new jsSHA(text, 'ASCII');
    return hmac.getHMAC(key, 'HEX', 'SHA-1', 'HEX');
}

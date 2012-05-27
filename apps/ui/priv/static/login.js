$('#login').click(function(ev) {
    $('#login').hide();
    $('.login input').prop('disabled', 'disabled');
    ev.preventDefault();

    var username = $('#username').val();
    var password = $('#password').val();

    function progress(s) {
	$('#progress').text(s);
    }
    function fail(s) {
	progress(s);
	$('.login input').prop('disabled', false);
	$('#login').show();
    }
    /* Request salt+challenge for hashing */
    progress("Obtaining challenge...");
    $.ajax({ type: 'POST',
	     data: {
		 username: username
	     },
	     url: '/login',
	     success: function(challenge) {
		 if (challenge && challenge.salt && challenge.token) {
		     progress("HMAC 1");
		     var salted = hmac(challenge.salt, password);
		     progress("HMAC 2");
		     var response = hmac(challenge.token, salted);
		     progress("Sending response");
		     $.ajax({ type: 'POST',
			      data: {
				  token: challenge.token,
				  response: response
			      },
			      url: '/login',
			      success: function(response) {
				  if (response && response.welcome) {
				      progress("Welcome to Bitlove!");
				      document.location = response.welcome;
				  } else {
				      fail((response && response.error) || "Cannot authenticate");
				  }
			      },
			      error: function() {
				  fail("Error sending response");
			      }
			    });
		     } else {
			 fail((challenge && challenge.error) || "Cannot login");
		     }
		 },
		 error: function() {
		     fail("Error obtaining challenge");
		 }
	     });
});

function hmac(key, text) {
    var hmac = new jsSHA(text, 'ASCII');
    return hmac.getHMAC(key, 'HEX', 'SHA-1', 'HEX');
}

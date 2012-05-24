function LightBox() {
    this.el_background = $("<div class='lightboxbackground'></div>");
    this.el = $("<div class='lightbox'></div>");
    this.el_background.append(this.el);
    $('body').append(this.el_background);
}
LightBox.prototype = {
    remove: function() {
	this.el.remove();
	this.el_background.remove();
    },
    find: function(key) {
	return this.el.find(key);
    },
    content: function(contents) {
	this.el.empty();
	this.el.append(contents);
    }
};

/**
 * Edit user details
 */

var editButton = $("<p class='edit button'>Edit</p>");
$('.meta').after(editButton);
var detailsPath = document.location.pathname + "/details.json";
editButton.bind('click', function() {
    var box = new LightBox();
    box.content('<p>Retrieving...</p>');

    $.ajax({ url: detailsPath,
	     success: function(response) {
		 box.content("<form class='useredit'>" +
			     "<h2>Edit user information</h2>" +
			     "<p><label for='title'>Title: <input id='title'></p>" +
			     "<p><label for='image'>Image link: <input id='image'></p>" +
			     "<p><label for='homepage'>Homepage: <input id='homepage'></p>" +
			     "<input type='reset' class='cancel button' value='Cancel'>" +
			     "<input type='submit' class='save button' value='Save'>" +
			     "</form>");
		 box.find('#title').val(response.title);
		 box.find('#image').val(response.image);
		 box.find('#homepage').val(response.homepage);
		 box.find('.cancel').click(box.remove.bind(box));
		 box.find('.save').click(function(ev) {
		     ev.preventDefault();

		     var data = {
			 title: box.find('#title').val(),
			 image: box.find('#image').val(),
			 homepage: box.find('#homepage').val()
		     };
		     box.content("<p>Submitting...</p>");
		     $.ajax({ type: 'POST',
			      url: detailsPath,
			      data: data,
			      success: function() {
				  /* Force refresh: */
				  document.location.search = "?" + Math.ceil(Math.random() * 999);
			      },
			      error: function() {
				  box.content("<p>Cannot submit</p>" +
					      "<p class='button'>Close</p>");
				  box.find('.button').click(box.remove.bind(box));
			      }
			    });
		 });
	     },
	     error: function() {
		 box.content("<p>An error occured</p>" +
			     "<p class='button'>Close</p>");
		 box.find('.button').click(box.remove.bind(box));
	     }
	   });
});

/**
 * Add feed button
 */
var addButton = $("<p class='add button'>Add</a>");
$('.col1').append(addButton);
addButton.bind('click', function() {
    var box = new LightBox();
    box.content("<form class='addfeed'>" +
		"<h2>Add a new podcast feed</h2>" +
		"<p><label for='slug'>Slug: <input id='slug'></p>" +
		"<p id='slughint' class='hint'></p>" +
		"<p><label for='url'>URL: <input id='url'></p>" +
		"<p class='hint'>All feeds are subject to manual confirmation.</p>" +
		"<input type='reset' class='cancel button' value='Cancel'>" +
		"<input type='submit' class='save button' value='Add'>" +
		"</form>");

    var slugEl = box.find('#slug');
    function fixSlug() {
	var s = slugEl.val().
	    toLowerCase().
	    replace(/[^0-9a-z\-_]/g, "");

	if (s !== slugEl.val())
	    slugEl.val(s);

	box.find('#slughint').
	    text((s.length > 0) ?
		 "http://bitlove.org" + document.location.pathname + "/" + s:
		 "");
    }
    slugEl.bind('change', fixSlug);
    slugEl.bind('input', fixSlug);
    slugEl.bind('keyup', fixSlug);

    box.find('.cancel').click(box.remove.bind(box));
    box.find('.save').click(function(ev) {
	ev.preventDefault();

	var slug = slugEl.val();
	var path = document.location.pathname + "/" + slug;
	var url = box.find('#url').val();
	box.content("<p>Adding your feed...</p>");

	$.ajax({ type: 'PUT',
		 url: path,
		 data: {
		     url: url
		 },
		 success: function(response) {
		     if (response && response.link) {
			 box.content("<p>Your feed has been created: <a class='link'></a></p>" +
				     "<p class='hint'>Your feeds are private by default. Don't forget to edit them if you are satisfied with what you're seeing. Contact us otherwise: <a href='mailto:mail@bitlove.org'>mail@bitlove.org</a>.</p>" +
				     "<p class='button'>Close</p>");
			 box.find('.link').attr('href', response.link);
			 box.find('.link').text(response.link);
			 box.find('.button').click(function() {
			     /* Force refresh: */
			     document.location.search = "?" + Math.ceil(Math.random() * 999);
			 });
		     } else {
			 box.content("<p class='message'></p>" +
				     "<p class='button'>Close</p>");
			 box.find('.message').text((response && response.error) || "An error occured");
			 box.find('.button').click(box.remove.bind(box));
		     }
		 },
		 error: function() {
		     box.content("<p>Cannot communicate</p>" +
				 "<p class='button'>Close</p>");
		     box.find('.button').click(box.remove.bind(box));
		 }
	       });
    });
});

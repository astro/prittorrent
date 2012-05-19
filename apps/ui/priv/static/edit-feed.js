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
 * Edit feed details
 */

var editButton = $("<p class='edit button'>Edit</p>");
$('.meta').before(editButton);
var detailsPath = document.location.pathname + "/details.json";
editButton.bind('click', function() {
    var box = new LightBox();
    box.content('<p>Retrieving...</p>');

    $.ajax({ url: detailsPath,
	     success: function(response) {
		 box.content("<form class='feededit'>" +
			     "<h2>Edit feed</h2>" +
			     "<p><input type='checkbox' id='public'> <label for='public'>Public</label> " +
			     "<span class='hint'>Show this feed on your user page and in public listings? You should enable this once everything works.</span></p>" +
			     "<p><input type='checkbox' id='settitle'> <label for='settitle'>Overwrite title</label></p>" +
			     "<p class='titleline'><label for='title'>Title:</label> <input id='title'></p>" +
			     "<input type='reset' class='cancel button' value='Cancel'>" +
			     "<input type='submit' class='save button' value='Save'>" +
			     "</form>");
		 box.find('#public').prop('checked', response.public);
		 box.find('#settitle').prop('checked', response.title && response.title.length > 0);
		 box.find('#title').val(response.title);
		 function titleVisibility() {
		     var titleline = box.find('.titleline');
		     if (box.find('#settitle').prop('checked'))
			 titleline.show();
		     else
			 titleline.hide();
		 }
		 box.find('#settitle').bind('change', titleVisibility);
		 titleVisibility();
		 box.find('.cancel').click(box.remove.bind(box));
		 box.find('.save').click(function(ev) {
		     ev.preventDefault();

		     var data = {
			 'public': box.find('#public').prop('checked')
		     };
		     if (box.find('#settitle').prop('checked'))
			 data.title = box.find('#title').val();
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
 * Remove feed button
 */
var rmButton = $("<p class='rm button'>Delete</p>");
editButton.before(rmButton);
rmButton.bind('click', function() {
    var box = new LightBox();
    box.content("<form>" +
		"<h2>Delete feed</h2>" +
		"<p class='hint'>You should not permanently remove feeds that your audience has subscribed. It is very inconvenient for them.</p>" +
		"<p>Are you sure?</p>" +
		"<input type='reset' class='cancel button' value='Cancel'>" +
		"<input type='submit' class='save button' value='Delete'>" +
		"</form>");
    box.find('.cancel').click(box.remove.bind(box));
    box.find('.save').click(function(ev) {
	ev.preventDefault();

	var path = document.location.pathname;
	$.ajax({ type: 'DELETE',
		 url: path,
		 success: function(response) {
		     box.content("<p>Rest in peace, little feed.</p>" +
				 "<p class='button'>Sorry</p>");
		     box.find('.button').click(function() {
			 box.remove();
			 if (response && response.link)
			     document.location = response.link;
		     });
		 },
		 error: function() {
		     box.content("<p>Cannot submit</p>" +
				 "<p class='button'>Close</p>");
		     box.find('.button').click(box.remove.bind(box));
		 }
	       });
    });
});

/**
 * Remove torrent buttons
 */
$('.download').each(function() {
    var download = $(this);
    var purgeButton = $("<p class='purge button'>Purge</p>");
    download.before(purgeButton);
    purgeButton.click(function() {
	var box = new LightBox();
	box.content("<form>" +
		    "<h2>Purge enclosure</h2>" +
		    "<p class='hint'>Use this to remove or regenerate a torrent, depending on whether the enclosure is still present in the feed.</p>" +
		    "<p>Are you sure?</p>" +
		    "<input type='reset' class='cancel button' value='Cancel'>" +
		    "<input type='submit' class='save button' value='Purge'>" +
		    "</form>");
	box.find('.cancel').click(box.remove.bind(box));
	box.find('.save').click(function(ev) {
	    ev.preventDefault();

	    var path = download.find('.torrent a').attr('href');
	    $.ajax({ type: 'DELETE',
		     url: path,
		     success: function(response) {
			 download.remove();
			 box.remove();
		     },
		     error: function() {
			 box.content("<p>Request failed</p>" +
				     "<p class='button'>Close</p>");
			 box.find('.button').click(box.remove.bind(box));
		     }
		   });
	});
    });
});

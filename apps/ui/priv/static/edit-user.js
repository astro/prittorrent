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

var editButton = $("<p class='edit button'>Edit</p>");
$('.meta').after(editButton);
var detailsPath = document.location.pathname + "/details.json";
editButton.bind('click', function() {
    var box = new LightBox();
    box.content('<p>Retrieving...</p>');

    $.ajax({ url: detailsPath,
	     success: function(response) {
		 box.content("<form class='useredit'>" +
			     "<p><label for='title'>Title: <input id='title'></p>" +
			     "<p><label for='image'>Image link: <input id='image'></p>" +
			     "<p><label for='homepage'>Homepage: <input id='homepage'></p>" +
			     "<p class='cancel button'>Cancel</p>" +
			     "<p class='save button'>Save</p>" +
			     "</form>");
		 box.find('#title').val(response.title);
		 box.find('#image').val(response.image);
		 box.find('#homepage').val(response.homepage);
		 box.find('.cancel').click(box.remove.bind(box));
		 box.find('.save').click(function() {
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

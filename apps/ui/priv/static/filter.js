(function() {

function FilterableItem(el) {
    this.el = el;
    this.lang = el.attr('xml:lang') || '';
    var types = [];
    el.find('.torrent a').each(function() {
	var type = $(this).data('type');
	/* Split MIME type */
	types.push(type ? type.split('/')[0] : '');
    });
    this.types = types;
}
FilterableItem.prototype = {
    applyMask: function(mask) {
	var langMatch = !mask.lang[this.lang];
	var typeMatch = this.types.some(function(type) {
	    return !mask.type[type];
	});

	if (typeMatch && langMatch)
	    this.show();
	else
	    this.hide();
    },
    show: function() {
	this.el.removeClass('filteredout');
    },
    hide: function() {
	this.el.addClass('filteredout');
    }
};

var filterItems = [];
$('.item').each(function() {
    var el = $(this);
    filterItems.push(new FilterableItem($(this)));
});
var Filter = {
    items: $('.item').map(function() {
	return new FilterableItem($(this));
    }),

    getAllTypes: function() {
	var types = {}, i, j;
	for(i = 0; i < Filter.items.length; i++)
	    for(j = 0; j < Filter.items[i].types.length; j++) {
		var type = Filter.items[i].types[j];
		if (!types.hasOwnProperty(type))
		    types[type] = 0;
		types[type]++;
	    }
	return types;
    },
    getAllLangs: function() {
	var langs = {}, i;
	for(i = 0; i < Filter.items.length; i++) {
	    var lang = Filter.items[i].lang;
	    if (!langs.hasOwnProperty(lang))
		langs[lang] = 0;
	    langs[lang]++;
	}
	return langs;
    },

    /* Inverted: */
    mask: {
	type: {},
	lang: {}
    },

    applyMask: function() {
	for(var i = 0; i < Filter.items.length; i++)
	    Filter.items[i].applyMask(Filter.mask);

	if (window.localStorage && window.localStorage.setItem)
	    window.localStorage.setItem('Prittorrent.UI.Filter.Mask', JSON.stringify(Filter.mask));
    }
};

try {
    if (window.localStorage && window.localStorage.getItem) {
	Filter.mask = JSON.parse(window.localStorage.getItem('Prittorrent.UI.Filter.Mask'));
	/* Repair: */
	if (!Filter.mask)
	    Filter.mask = {};
	if (!Filter.mask.hasOwnProperty('type'))
	    Filter.mask.type = {};
	if (!Filter.mask.hasOwnProperty('lang'))
	    Filter.mask.lang = {};

	Filter.applyMask();
    }

} catch (x) {
    if (window.console && window.console.error)
	window.console.error(x.stack || x);
}

function sortKeysNullLast(o) {
    return Object.keys(o).sort(function(a, b) {
	if (!a && b)
	    return 1;
	else if (a && !b)
	    return -1;
	else if (a < b)
	    return -1;
	else if (a > b)
	    return 1;
	else
	    return 0;
    });
}

function FilterDialog() {
    this.el = $('<form class="filterdialog"><div class="type"><h2>By type</h2><ul></ul><p><a class="all">Select all</a></p></div><div class="lang"><h2>By language</h2><ul></ul><p><a class="all">Select all</a></p></div></form>');

    /* Add checkboxes  */
    var allTypes = Filter.getAllTypes();
    sortKeysNullLast(allTypes).forEach(function(type) {
	var text = type ?
	    (type.substr(0, 1).toLocaleUpperCase() + type.substr(1)) :
	    "Other";
	this.addOption('type', type, text, allTypes[type]);
    }.bind(this));
    var allLangs = Filter.getAllLangs();
    sortKeysNullLast(allLangs).forEach(function(lang) {
	var text = lang ?
	    (lang.substr(0, 1).toLocaleUpperCase() + lang.substr(1)) :
	    "Other";
	this.addOption('lang', lang, text, allLangs[lang]);
    }.bind(this));

    /* Select all */
    var update = this.update.bind(this);
    this.el.find('.type, .lang').each(function() {
	var col = $(this);
	col.find('.all').click(function(ev) {
	    ev.preventDefault();

	    col.find('input[type=checkbox]').prop('checked', true);
	    update();
	});
    });
}
FilterDialog.prototype = {
    hide: function() {
	this.el.remove();
    },
    addOption: function(column, val, text, count) {
	var li = $('<li></li>');

	var option = $('<input type="checkbox">');
	var id = "ft-" + column + "-" + val;
	option.attr('id', id);
	option.val(val);
	option.prop('checked', !Filter.mask[column][val]);
	option.change(function() {
	    this.update();
	}.bind(this));
	li.append(option);

	var label = $('<label></label>');
	label.attr('for', id);
	label.text(text || val);
	label.append(' <span class="counter"></span>');
	label.find('.counter').text("(" + count + ")");
	li.append(label);

	this.el.find('.' + column).find('ul').append(li);
    },
    update: function() {
	['type', 'lang'].forEach(function(column) {
	    /* Rebuild mask */
	    this.el.find('.' + column + ' input[type=checkbox]').each(function() {
		var input = $(this);
		Filter.mask[column][input.val()] = !input.prop('checked');
	    });
	}.bind(this));

	Filter.applyMask();
    }
};

var container = $('<div class="filter"></div>');
$('.feedslist').after(container);
var filterButton = $('<a class="filterbutton">Filter</a>');
container.append(filterButton);

var filterDialog;
filterButton.click(function(ev) {
    ev.preventDefault();

    if (!filterDialog) {
	filterDialog = new FilterDialog();
	container.append(filterDialog.el);
	filterButton.addClass('toggled');
    } else {
	filterDialog.hide();
	filterDialog = null;
	filterButton.removeClass('toggled');
    }
});

})();

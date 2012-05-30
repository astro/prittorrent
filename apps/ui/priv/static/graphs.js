function Graph(basepath, type) {
    this.type = type;
    this.el = $('<li class="graph"><h4></h4></li>');
    this.el.hide();
    switch(type) {
	case 'swarm':
	    this.el.find('h4').text("Swarm");
	    break;
	case 'traffic':
	    this.el.find('h4').text("Peer traffic");
	    break;
	case 'downloads':
	    this.el.find('h4').text("Complete downloads");
	    break;
    }

    $.ajax({ url: basepath + "/g/month/" + type + ".json",
	     success: this.setData.bind(this),
	     error: this.el.text.bind(this.el, "Error retrieving " + type + ".json")
	   });
}

Graph.prototype = {};

Graph.prototype.setData = function(data) {
    /* Prepare data */
    var i;
    var ds = [];
    for(var name in data) {
	var line = data[name];
	var d = Object.keys(line).sort().map(function(k) {
	    var v = line[k];
	    return [new Date(k).getTime(), v];
	});
	ds.push({ label: name,
		  data: d
		});
    }

    var tickFormatter = (this.type != 'traffic') ?
	function(value) {
	    return "" + Math.round(value);
	} :
	function(value) {
	    var units = ["", "K", "M", "G", "T"];
	    var u = 0;
	    while(value >= 1000 && u < units.length - 1) {
		value /= 1000;
		u++;
	    }
	    return Math.round(value) + " " + units[u] + "B/s";
	};

    var width = this.el.parent().innerWidth() || 400;
    /* Attach */
    var placeholder = $('<div style="width: ' + width + 'px; height: 200px"></div>');
    this.el.append(placeholder);
    $.plot(placeholder, ds, {
	xaxis: {
	    mode: "time",
	    timeformat: "%m-%d\n%H:%M",
	    tickLength: 7
	},
	yaxis: {
	    tickFormatter: tickFormatter
	},
	legend: {
	    show: true
	},
	hoverable: true,
	clickable: true
    });
    this.el.slideDown(200);
};

Graph.prototype.remove = function() {
    this.el.slideUp(300, this.el.remove.bind(this.el));
};

function StatsHook(basepath, stats) {
    this.basepath = basepath;
    this.stats = stats;
    this.graphs = {};

    this.attach('.seeders', 'swarm');
    this.attach('.leechers', 'traffic');
    this.attach('.downloads', 'downloads');
}

StatsHook.prototype = {};

StatsHook.prototype.attach = function(sel, type) {
    var toggle = this.stats.find(sel);
    toggle.addClass('toggleable');
    toggle.click(this.toggleGraph.bind(this, type, toggle));
};

StatsHook.prototype.toggleGraph = function(type, toggle) {
    if (this.graphs.hasOwnProperty(type)) {
	toggle.removeClass('toggled');
	this.graphs[type].remove();
	delete this.graphs[type];
    } else {
	toggle.addClass('toggled');
	var graph = this.graphs[type] = new Graph(this.basepath, type);
	this.stats.after(graph.el);
    }
};

$('.download').each(function() {
    var download = $(this);
    var m = download.find('.torrent a').attr('href').
	 match(/^(\/.+?)\.torrent$/);
    var basepath = m && m[1];
    var stats = download.find('.stats');
    if (basepath && stats)
	new StatsHook(basepath, stats);
});

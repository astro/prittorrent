function Graph(basepath, type) {
    this.basepath = basepath;
    this.type = type;

    this.el = $('<li class="graph"><select class="timeselect"><option value="day">Day</option><option value="week">Week</option><option value="month" selected>Month</option><option value="year">Year</option></select><h4></h4><div class="placeholder"></div></li>');
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

    var timeselect = this.el.find('.timeselect');
    var that = this;
    timeselect.change(function() {
	that.loadGraph();
    });

    this.loadGraph();
}

Graph.prototype = {};

Graph.prototype.loadGraph = function() {
    var timespec = this.el.find('.timeselect').val();
    var url = this.basepath + "/g/" + timespec + "/" + this.type + ".json";
    $.ajax({ url: url,
	     success: this.setData.bind(this),
	     error: this.el.text.bind(this.el, "Error retrieving " + url)
	   });
};

Graph.prototype.setData = function(response) {
    if (this.plot) {
	this.plot.shutdown();
	delete this.plot;
    }

    /* Prepare data */
    var i;
    var data = [];
    var type = this.type;
    var interval = response.interval * 1000;
    for(var name in response) {
	if (name == 'interval')
	    continue;

	var line = response[name];
	var d = Object.keys(line).sort().map(function(k) {
	    var v = line[k];
	    return [new Date(k).getTime(), v];
	});

	var label = name;
	var series = {
	    label: name,
	    data: d
	};
	switch(type) {
	    case 'swarm':
		series.label = "Downloads";
		switch(name) {
		    case 'seeders':
			series.label = "Seeders";
			break;
		    case 'leechers':
			series.label = "Leechers";
			break;
		}
		break;
	    case 'traffic':
		series.bars = { show: true,
				barWidth: interval
			      };
		switch(name) {
		    case 'down':
			series.label = "Downloaded";
			series.color = '#1f4faf';
			series.bars.fillColor = '#3fafef';
			break;
		    case 'up':
			series.label = "Uploaded";
			series.color = '#4faf1f';
			series.bars.fillColor = '#afef3f';
			break;
		    case 'up_seeder':
			series.label = "Uploaded by Bitlove";
			series.color = '#afaf1f';
			series.bars.fillColor = '#efef3f';
			break;
		}
		break;
	    case 'downloads':
		series.label = "Downloads";
		series.color = '#5faf1f';
		series.bars = { show: true,
				barWidth: interval,
				fillColor: '#afef3f'
			      };
		break;
	}

	data.push(series);
    }

    var tickFormatter = (type != 'traffic') ?
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
	    return Math.round(value) + " " + units[u] + "B";
	};

    /* Attach */
    var width = this.el.parent().innerWidth() || 400;
    var placeholder = this.el.find('.placeholder');
    placeholder.attr('style', "width: " + width + "px; height: 200px");
    this.plot = $.plot(placeholder, data, {
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
	graph.el.hide();
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

-module(ui_widget).

-export([serve_base/0, serve_podpress/0, serve_powerpress/0]).

serve_base() ->
    [
     <<"window.torrentByEnclosure = ">>,
     wrap_fun(base_code()),
     <<";\n">>
    ].

serve_podpress() ->
    [
     wrap_fun(podpress_code()),
     <<";\n">>
    ].


serve_powerpress() ->
    [
     wrap_fun(powerpress_code()),
     <<";\n">>
    ].


wrap_fun(Code) ->
    [<<"(function() {\n">>, Code, <<"\n})()">>].


base_code() ->
    <<"
var XHR = window.AnonXMLHttpRequest || window.XMLHttpRequest;
if (!XHR)
    throw new Error(\"Your browser lacks support for standardized AJAX calls. What the ...?\");


var urlCallbacks = {};

function resolve(url, cb) {
    if (urlCallbacks.hasOwnProperty(url)) {
	urlCallbacks[url].push(cb);
    } else {
	urlCallbacks[url] = [cb];
    }

    maySetTimeout();
}

var timeout;
function maySetTimeout() {
    var pending = Object.keys(urlCallbacks).length > 0;

    if (!timeout && pending) {
	timeout = setTimeout(function() {
	    timeout = undefined;
	    maySend();
	}, 50);
    } else if (timeout && !pending) {
	clearTimeout(timeout);
    }
}

function maySend() {
    var urls = Object.keys(urlCallbacks).slice(0, 8);
    if (urls.length > 0) {
	doSend(urls, function(response) {
	    /* Dispatch to callbacks */
	    urls.forEach(function(url) {
		var urlResponse = response[url];
		var cbs = urlCallbacks[url];
		delete urlCallbacks[url];
		cbs.forEach(function(cb) {
		    try {
			cb(urlResponse);
		    } catch(e) {
			if (console && console.error)
			    console.error(e && e.stack || e);
		    }
		});
	    });
	});
    }
}

function doSend(urls, cb) {
    var q = urls.map(function(url) {
	return \"url=\" + encodeURIComponent(url);
    }).join(\"&\");

    var cl = new XHR();
    cl.open('GET', 'http://api.bitlove.org/by-enclosure.json?' + q);
    cl.onreadystatechange = function() {
	if (this.readyState == this.DONE) {
	    var response;
	    if (this.status == 200 &&
		this.responseText) {
		try {
		    response = JSON.parse(this.responseText);
		} catch (e) {
		    if (console && console.error)
			console.error(e && e.stack || e);
		}
	    }
	    cb(response);

	    /* Continue with next batch: */
	    maySend();
	}
    };
    cl.send();
}

">>.

podpress_code() ->
    [
     base_code(),
     <<"
var $ = jQuery;
$(document).ready(function() {
    /* For Podpress */
    $('.podPress_downloadlinks').each(function() {
	var orig = $(this);
	var url = orig.find('a').attr('href');

	if (/\.torrent$/.test(url))
	    return;

	resolve(url, function(info) {
	    console.log(\"resolve\",url,info);
	    var torrent = info && info.sources && info.sources[0] && info.sources[0].torrent;
	    if (info && torrent) {
		var t = $('<div class=\"podPress_downloadlinks\"><a class=\"podpress_downloadimglink\" title=\"Download via BitTorrent\" type=\"application/x-bittorrent\"><img src=\"http://bitlove.org/static/bitlove-button.png\" class=\"podPress_imgicon\"></a><span class=\"podpress_mediafile_title\"><span class=\"s\"></span><span class=\"l\"></span><span class=\"d\"></span></span></div>');
		t.find('a').attr('href', torrent);
		t.find('.s').text(info.seeders + \" Seeders, \");
		t.find('.l').text(info.leechers + \" Leechers\");
		if (info.downloaded == 1) {
		    t.find('.d').text(\", 1 Download\");
		} else if (info.downloaded > 1) {
		    t.find('.d').text(\", \" + info.downloaded + \" Downloads\");
		}

		var link = info.sources[0].permalink;
		if (link) {
		    var source = $('<span class=\"podpress_mediafile_dursize\">on <a>Bitlove</a></span>');
		    source.find('a').attr('href', link);
		    t.append(source);
		}

		orig.after(t);
	    }
	});
    });
});
">>].

powerpress_code() ->
    [
     base_code(),
     <<"

document.addEventListener('DOMContentLoaded', function(){
    var i, ps = document.getElementsByClassName('powerpress_links');
    for(i = 0; i < ps.length; i++) {
	var p = ps[i];
	var j, as = p.getElementsByClassName('powerpress_link_d');
	for(j = 0; j < as.length; j++) {
	    var a = as[j];
	    var url = a.getAttribute('href');

	    if (/\.torrent$/.test(url))
		return;

	    (function(p) {
	        resolve(url, function(info) {
	            console.log(\"resolve\",url,info);
	            var torrent = info && info.sources && info.sources[0] && info.sources[0].torrent;
	            if (info && torrent) {
		        var t1 = document.createTextNode(\" | \");
		        p.appendChild(t1);

		        var a1 = document.createElement('a');
		        a1.textContent = \"Torrent\";
		        a1.setAttribute('href', torrent);
		        a1.setAttribute('type', \"application/x-bittorrent\");
		        p.appendChild(a1);

		        var link = info.sources[0].permalink;
		        if (link) {
		            var t2 = document.createTextNode(\" on \");
		            p.appendChild(t2);

		            var a2 = document.createElement('a');
		            a2.textContent = \"Bitlove\";
		            a2.setAttribute('href', link);
		            p.appendChild(a2);
		        }
	            }
	        });
	    })(p);
	}
    }
}, false);
">>].
    

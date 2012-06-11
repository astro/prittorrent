-record(req, {method, path,
	      qs, body,
	      languages,
	      sid, session_user
	     }).

-define(NS_ATOM, <<"http://www.w3.org/2005/Atom">>).

-define(MIME_TORRENT, <<"application/x-bittorrent">>).
-define(MIME_RSS, <<"application/rss+xml">>).
-define(MIME_ATOM, <<"application/atom+xml">>).
-define(MIME_JSON, <<"application/json">>).
-define(MIME_OPML, <<"text/x-opml">>).
-define(MIME_JAVASCRIPT, <<"text/javascript">>).

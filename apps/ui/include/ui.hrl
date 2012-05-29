-record(req, {method, path,
	      body,
	      encodings, languages,
	      sid, session_user
	     }).

-define(NS_ATOM, <<"http://www.w3.org/2005/Atom">>).

-define(MIME_TORRENT, <<"application/x-bittorrent">>).
-define(MIME_RSS, <<"application/atom+xml">>).
-define(MIME_ATOM, <<"application/rss+xml">>).



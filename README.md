PritTorrent
===========

Build
-----

```
rebar get-deps compile generate
```


TODO
----

* http://bitlove.org/channelcast + Flattr error?
* UI: Detect browser language
* Zlib for UI
* lhttpc + https! + zlib

* Login

  - Signup
  - Session management
  - Logged in indicator
  - Edit user
  - List feeds (status)
  - Add/Edit feeds
  - Private feeds
  - cowboy + ssl, enforce https for login

* Check U-A & replace RSS links with Miro subscribe URLs

* Feed summaries: X items, Y torrents
* <atom:link rel="self">

* Embedabble Widget

* Storage app
  - Avoid dup connections to 1 HTTP server (IP)
  - Fair queueing
  - Caching
* URL longener?
* OEmbed
* Widgets
* API

* Stats:
  - Render pretty graphs
  - DLs by country/client?

Future features:

* UDP tracker
* UTP wire protocol
* UI: Wholesome OPML export
* Super-seeding
* Slot queues
* PEX
* DHT support

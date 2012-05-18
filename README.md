PritTorrent
===========

Build
-----

```
rebar get-deps compile generate
```


TODO
----

* Multiple front pages
* http://bitlove.org/channelcast + Flattr error?
* UI: Detect browser language
* Zlib for UI
* lhttpc + https! + zlib

* Login

  - List feeds (status)
  - Edit feeds:
    * Overwrite title
    * Set Private
    * Delete torrents
    * Feed & enclosure status
  - Private feeds
  - Torrentify flag
  - cowboy + ssl, enforce https for login
  - more configurability


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

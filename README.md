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
* UI: Detect browser language
* Zlib for UI
* lhttpc + https! + zlib

* Login

  - Edit feeds:
    * Torrentify and HTTP status
    * Enclosure HTTP status
  - enforce https for log in
  - more configurability


* Check U-A & replace RSS links with Miro subscribe URLs

* Feed summaries: X items, Y torrents
* <atom:link rel="self">

* Embedable Widget

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

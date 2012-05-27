PritTorrent
===========

Build
-----

```
rebar get-deps compile generate
```


TODO
----

* model:
  * queueify scraped triggers
  * composite feeds

* style:
  * Fonts
  * Flattr donate

* API
* Composite feeds (map item ids)

* Check U-A & replace RSS links with Miro subscribe URLs
* UI: Detect browser language
* Zlib for UI
* lhttpc + https! + zlib

* Edit feeds:
  * Add & fetch immediately
  * HTTP status
  * Enclosure HTTP status
  * Style pending feeds

* update cowboy
* feeds_parse: http://video.search.yahoo.com/mrss

* enforce https for log in
* more configurability

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

* Stats:
  - Render pretty graphs
  - DLs by country/client?

* Rehash on detected enclosure data change
* Multiple sources per feed

Future features:

* UDP tracker
* UTP wire protocol
* UI: Wholesome OPML export
* Super-seeding
* Slot queues
* PEX
* DHT support

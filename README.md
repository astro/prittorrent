PritTorrent
===========

Build
-----

```
rebar get-deps compile generate
```


TODO
----

* <atom:link rel="self">

* graphs:
  * refactor
  * stacked traffic

* model:
  * queueify scraped triggers

* style:
  * Fonts
  * Flattr donate

* API

* Check U-A & replace RSS links with Miro subscribe URLs
* UI: Detect browser language
* Zlib for UI
* lhttpc + zlib
* parse rss pubDate tz offset

* Edit user:
  * About field
* Edit feeds:
  * Add & fetch immediately
  * HTTP status
  * Enclosure HTTP status
  * Style pending feeds

* update cowboy
* feeds_parse: http://video.search.yahoo.com/mrss

* enforce https for log in
* more configurability

* Fetch & display feed summaries
* Download buttons: display mime type/titles

* Feed summaries: X items, Y torrents

* Embedable Widget

* Storage app
  - Avoid dup connections to 1 HTTP server (IP)
  - Fair queueing
  - Caching
* URL longener?
* OEmbed
* Widgets

* Stats:
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

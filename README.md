PritTorrent
===========

Build
-----

```
rebar get-deps compile generate
```


TODO
----

* limit Published

* enforce https for log in
* clickable stats hint
* UI: Detect browser language
* Scaled images cache

* <atom:link rel="self">
* parse rss pubDate tz offset
* Download buttons: display mime type/titles

* refactor xml storage
* stop seeding
* model:
  * queueify scraped triggers

* style:
  * Fonts
  * Flattr donate

* Check U-A & replace RSS links with Miro subscribe URLs
* lhttpc + zlib

* Edit user:
  * About field
* Edit feeds:
  * Add & fetch immediately

* graphs:
  * refactor
  * stacked traffic

* update cowboy
* feeds_parse: http://video.search.yahoo.com/mrss

* more configurability

* Fetch & display feed summaries

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

PritTorrent
===========

Build
-----

```
rebar get-deps compile generate
```


TODO
----

* URL longener?
* Fix empty downloads.type
* filter.js:
  * Fix button style breakage in Mozilla
  * Fix z-index (Android?)
* New {feeds,downloads}.{lang,summary,type} in:
  * Downloads Feeds
  * HTML

* enforce https for log in
* clickable stats hint

* <atom:link rel="self">
* <atom:link rel="alternate">

* stop seeding

* Check U-A & replace RSS links with Miro subscribe URLs

* Edit user:
  * About field
* Edit feeds:
  * Add & fetch immediately

* feeds_parse: http://video.search.yahoo.com/mrss

* more configurability

* Fetch & display feed summaries

* Feed summaries: X items, Y torrents

* Storage app
  - Avoid dup connections to 1 HTTP server (IP)
  - Fair queueing
  - Caching
* OEmbed

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

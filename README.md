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

* Feed summaries: X items, Y torrents
* <atom:link rel="self">

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

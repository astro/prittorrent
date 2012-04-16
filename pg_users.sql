CREATE TABLE users ("name" TEXT NOT NULL,
       	     	    "email" TEXT NOT NULL,
		    "password" TEXT NOT NULL,
		    "activated" BOOLEAN DEFAULT FALSE,
		    "title" TEXT,
		    "homepage" TEXT,
		    PRIMARY KEY ("name"));
CREATE VIEW activated_users AS
       SELECT "name", "email", "password"
       FROM users
       WHERE "activated";

CREATE TABLE feeds ("url" TEXT NOT NULL,
       	     	    "last_update" TIMESTAMP,
       	     	    "etag" TEXT,
		    "last_modified" TEXT,
		    "error" TEXT,
		    "title" TEXT,
		    "homepage" TEXT,
		    "xml" TEXT,
       	     	    PRIMARY KEY ("url"));

CREATE TABLE user_feeds ("user" TEXT NOT NULL REFERENCES "users" ("name"),
       	     		 "slug" TEXT NOT NULL,
       	     		 "feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
			 PRIMARY KEY ("user", "slug", "feed"));


CREATE TABLE feed_items ("feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
       	     		 "id" TEXT NOT NULL,
			 "title" TEXT,
			 "homepage" TEXT,
			 "published" TIMESTAMP NOT NULL,
			 "payment" TEXT,
			 "updated" TIMESTAMP,
			 "xml" TEXT,
			 PRIMARY KEY ("feed", "id"));

CREATE TABLE enclosures ("feed" TEXT NOT NULL,
       	     		 "item" TEXT NOT NULL,
			 "url" TEXT NOT NULL,
			 PRIMARY KEY ("feed", "item", "url"),
			 FOREIGN KEY ("feed", "item") REFERENCES "feed_items" ("feed", "id"));

CREATE TABLE enclosure_torrents ("url" TEXT NOT NULL PRIMARY KEY,
       	     			 last_update TIMESTAMP,
				 error TEXT,
				 info_hash BYTEA);
CREATE VIEW enclosures_to_hash AS
       SELECT enclosures.url,
       	      enclosure_torrents.last_update AS last_update,
	      enclosure_torrents.error AS error,
	      enclosure_torrents.info_hash
	       FROM enclosures LEFT JOIN enclosure_torrents
	       ON (enclosures.url=enclosure_torrents.url)
	        WHERE enclosure_torrents.info_hash IS NULL
		OR LENGTH(enclosure_torrents.info_hash)=0 ORDER BY last_update;

CREATE TABLE torrents ("info_hash" BYTEA PRIMARY KEY,
       	     	       "torrent" BYTEA);

CREATE VIEW item_torrents AS
       SELECT enclosures.feed, enclosures.item, enclosures.url,
       	      enclosure_torrents.info_hash
       FROM enclosure_torrents LEFT JOIN enclosures ON (enclosures.url=enclosure_torrents.url)
       WHERE LENGTH(info_hash)=20;

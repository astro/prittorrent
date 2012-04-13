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
       	     		 "feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
			 PRIMARY KEY ("user", "feed"));


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
-- TODO: order
CREATE VIEW enclosures_to_hash AS
       (SELECT DISTINCT "url" FROM enclosures
       	       WHERE "url" NOT IN
	       (SELECT "url" FROM enclosure_torrents WHERE "last_update" > CURRENT_TIMESTAMP - '1 day'::interval));

CREATE TABLE torrents ("info_hash" BYTEA PRIMARY KEY,
       	     	       "torrent" BYTEA);

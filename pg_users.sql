CREATE TABLE users ("name" TEXT NOT NULL,
       	     	    "email" TEXT NOT NULL,
		    "password" TEXT NOT NULL,
		    "activated" BOOLEAN DEFAULT FALSE,
		    "title" TEXT,
		    "image" TEXT,
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
		    "image" TEXT,
		    "xml" TEXT,
       	     	    PRIMARY KEY ("url"));

CREATE TABLE user_feeds ("user" TEXT NOT NULL REFERENCES "users" ("name"),
       	     		 "slug" TEXT NOT NULL,
       	     		 "feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
			 PRIMARY KEY ("user", "slug", "feed"));

CREATE OR REPLACE FUNCTION feed_to_update(
       update_interval INTERVAL,
       OUT next_url TEXT, OUT wait INTERVAL
   ) RETURNS RECORD AS $$
    DECLARE
	next_feed RECORD;
    BEGIN
	LOCK "feeds" IN SHARE ROW EXCLUSIVE MODE;
        SELECT "url", "last_update"
	  INTO next_feed
          FROM "feeds"
      ORDER BY "last_update" ASC NULLS FIRST
         LIMIT 1;

	next_url := next_feed.url;
	IF next_feed.last_update IS NULL THEN
	   next_feed.last_update = '1970-01-01 00:00:00';
	END IF;
	wait := next_feed.last_update + update_interval - CURRENT_TIMESTAMP;

	IF wait <= '0'::INTERVAL THEN
	   UPDATE "feeds"
	      SET "last_update"=CURRENT_TIMESTAMP
	    WHERE "url"=next_url;
	END IF;
    END;
$$ LANGUAGE plpgsql;


CREATE TABLE feed_items ("feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
       	     		 "id" TEXT NOT NULL,
			 "title" TEXT,
			 "homepage" TEXT,
			 "published" TIMESTAMP NOT NULL,
			 "payment" TEXT,
			 "image" TEXT,
			 "updated" TIMESTAMP,
			 "xml" TEXT,
			 PRIMARY KEY ("feed", "id"));

CREATE INDEX feed_items_published ON feed_items ("published");

CREATE OR REPLACE FUNCTION feed_items_ensure_image() RETURNS trigger AS $$
    BEGIN
        IF NEW.image IS NULL OR NEW.image='' THEN
            SELECT image INTO NEW.image FROM feeds WHERE url=NEW.feed;
        END IF;
        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER feed_items_ensure_image BEFORE INSERT OR UPDATE ON feed_items
    FOR EACH ROW EXECUTE PROCEDURE feed_items_ensure_image();

CREATE TABLE enclosures ("feed" TEXT NOT NULL,
       	     		 "item" TEXT NOT NULL,
			 "url" TEXT NOT NULL,
			 PRIMARY KEY ("feed", "item", "url"),
			 FOREIGN KEY ("feed", "item") REFERENCES "feed_items" ("feed", "id"));
CREATE INDEX enclosures_url ON enclosures ("url");

CREATE TABLE enclosure_torrents ("url" TEXT NOT NULL PRIMARY KEY,
       	     			 last_update TIMESTAMP,
				 error TEXT,
				 info_hash BYTEA);
CREATE INDEX enclosure_torrents_info_hash ON enclosure_torrents ((length(info_hash)), info_hash);
CREATE VIEW enclosures_to_hash AS
       SELECT enclosures.url,
              enclosure_torrents.last_update AS last_update,
              enclosure_torrents.error AS error,
              enclosure_torrents.info_hash
               FROM enclosures LEFT JOIN enclosure_torrents
               ON (enclosures.url=enclosure_torrents.url)
                WHERE enclosure_torrents.info_hash IS NULL
                OR LENGTH(enclosure_torrents.info_hash)=0 ORDER BY last_update NULLS FIRST;

CREATE OR REPLACE FUNCTION enclosure_to_hash(
       min_inactivity INTERVAL DEFAULT '2 hours',
       OUT enclosure_url TEXT
   ) RETURNS TEXT AS $$
    DECLARE
        next_url RECORD;
    BEGIN
        LOCK "enclosure_torrents" IN SHARE ROW EXCLUSIVE MODE;
        SELECT enclosures_to_hash.url, enclosures_to_hash.last_update
          INTO next_url
          FROM enclosures_to_hash
         LIMIT 1;

        IF next_url.last_update IS NULL THEN
            next_url.last_update = '1970-01-01 00:00:00';
        END IF;
        IF next_url.last_update <= CURRENT_TIMESTAMP - min_inactivity THEN
           enclosure_url := next_url.url;
           IF EXISTS (SELECT "url" FROM enclosure_torrents WHERE "url"=enclosure_url) THEN
               UPDATE enclosure_torrents SET "last_update"=CURRENT_TIMESTAMP WHERE "url"=enclosure_url;
           ELSE
               INSERT INTO enclosure_torrents ("url", "last_update") VALUES (enclosure_url, CURRENT_TIMESTAMP);
           END IF;
        END IF;
    END;
$$ LANGUAGE plpgsql;

CREATE VIEW torrentified AS
       SELECT enclosures.url,
       	      enclosure_torrents.last_update AS last_update,
	      enclosure_torrents.error AS error,
	      enclosure_torrents.info_hash
	       FROM enclosures LEFT JOIN enclosure_torrents
	       ON (enclosures.url=enclosure_torrents.url)
	        WHERE LENGTH(enclosure_torrents.info_hash)=20 ORDER BY last_update;

CREATE TABLE torrents ("info_hash" BYTEA PRIMARY KEY,
       	     	       "name" TEXT,
		       "size" BIGINT,
       	     	       "torrent" BYTEA);

CREATE VIEW item_torrents AS
       SELECT enclosures.feed, enclosures.item, enclosures.url,
       	      enclosure_torrents.info_hash
       FROM enclosure_torrents LEFT JOIN enclosures ON (enclosures.url=enclosure_torrents.url)
       WHERE LENGTH(info_hash)=20;

CREATE OR REPLACE VIEW torrentified_items AS
       SELECT *
       FROM feed_items
       WHERE EXISTS
       	     (SELECT "url"
	      FROM enclosures
	      WHERE "feed"=feed_items.feed
 	        AND "item"=feed_items.id
		AND "url" IN
		    (SELECT "url" FROM torrentified)
	     ) ORDER BY "published" DESC;


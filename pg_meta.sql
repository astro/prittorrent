CREATE TABLE users ("name" TEXT NOT NULL,
       	     	    "email" TEXT NOT NULL,
		    "salt" TEXT NOT NULL,
		    "salted" TEXT,
		    "activation" TEXT,
		    "title" TEXT,
		    "image" TEXT,
		    "homepage" TEXT,
		    PRIMARY KEY ("name"));
-- CREATE VIEW activated_users AS
--        SELECT "name", "email", "password"
--        FROM users
--        WHERE "activated";

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


-- Check this with: select * from enclosure_torrents where info_hash not in (select info_hash from torrents);
-- Or add a constraint on info_hash with either NULL or FOREIGN KEY torrents (info_hash)
CREATE TABLE enclosure_torrents ("url" TEXT NOT NULL PRIMARY KEY,
       	     			 last_update TIMESTAMP,
				 error TEXT,
				 info_hash BYTEA);

CREATE TABLE torrents ("info_hash" BYTEA PRIMARY KEY,
       	     	       "name" TEXT,
		       "size" BIGINT,
       	     	       "torrent" BYTEA);



-- Login UI
CREATE TABLE login_tokens (
       "user" TEXT,
       "token" BYTEA PRIMARY KEY,
       "created" TIMESTAMP
);

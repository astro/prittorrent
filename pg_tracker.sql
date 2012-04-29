-- fkey ensures we only track for known torrents:
CREATE TABLE tracked ("info_hash" BYTEA NOT NULL REFERENCES torrents("info_hash"),
       	     	      "peer_id" BYTEA NOT NULL,
		      "host" BYTEA NOT NULL,
		      "port" INT NOT NULL,
		      "uploaded" BIGINT,
		      "downloaded" BIGINT,
		      "downspeed" BIGINT,
		      "upspeed" BIGINT,
		      "left" BIGINT,
		      "last_request" TIMESTAMP NOT NULL,
		      PRIMARY KEY ("info_hash", "peer_id")
		     );

-- Not neccessary due to tracked PKEY?
--CREATE INDEX tracked_info_hash ON tracked ("info_hash");

-- For leechers:
CREATE VIEW tracker AS
       SELECT "info_hash", "peer_id", "host", "port"
         FROM tracked
     ORDER BY "left" ASC, "last_request" DESC
        LIMIT 60;

-- For seeders:
CREATE VIEW tracker_leechers AS
       SELECT "info_hash", "peer_id", "host", "port"
         FROM tracked
        WHERE "left">0
     ORDER BY "left" ASC, "last_request" DESC
        LIMIT 60;

CREATE OR REPLACE FUNCTION set_peer(
       "p_info_hash" BYTEA, "p_host" BYTEA, "p_port" INT, "p_peer_id" BYTEA,
       "p_uploaded" BIGINT, "p_downloaded" BIGINT, "p_left" BIGINT,
       OUT "up" BIGINT, OUT "down" BIGINT
    ) RETURNS RECORD AS $$
    DECLARE
        "old" RECORD;
        "old_age" FLOAT;
        "up" BIGINT;
        "down" BIGINT;
        "p_upspeed" BIGINT;
        "p_downspeed" BIGINT;
    BEGIN
        SELECT * INTO "old" FROM tracked WHERE "info_hash"="p_info_hash" AND "peer_id"="p_peer_id";
        IF "old" IS NULL THEN
            INSERT INTO tracked ("info_hash", "peer_id", "host", "port",
                                 "uploaded", "downloaded", "left", "last_request")
                VALUES ("p_info_hash", "p_peer_id", "p_host", "p_port",
                        "p_uploaded", "p_downloaded", "p_left", now());
        ELSE
            "old_age" := EXTRACT(EPOCH FROM (now() - old.last_request));
            -- Estimate speeds, with sanity checks first:
            IF "old_age" <= 30 * 60 AND
               "p_uploaded" >= old.uploaded AND
               "p_downloaded" >= old.downloaded AND
               "p_left" <= old.left THEN
                "up" := "p_uploaded" - old.uploaded;
                "down" := "p_downloaded" - old.downloaded;
                "p_upspeed" := (up / "old_age")::BIGINT;
                "p_downspeed" := (down / "old_age")::BIGINT;
            END IF;

            UPDATE tracked SET "host"="p_host", "port"="p_port",
                               "uploaded"="p_uploaded", "downloaded"="p_downloaded",
                               "left"="p_left", "last_request"=now(),
                               "upspeed"="p_upspeed", "downspeed"="p_downspeed"
             WHERE "info_hash"="p_info_hash" AND "peer_id"="p_peer_id";
        END IF;
    END;
$$ LANGUAGE plpgsql;

-- periodic tracked cleaner
CREATE OR REPLACE FUNCTION clear_peers(maxage INTERVAL) RETURNS void AS $$
    BEGIN
        DELETE FROM tracked WHERE "last_request" <= CURRENT_TIMESTAMP - maxage;
    END;
$$ LANGUAGE plpgsql;

CREATE TABLE scraped (
       "info_hash" BYTEA NOT NULL PRIMARY KEY,
       "seeders" INT,
       "leechers" INT,
       "upspeed" BIGINT,
       "downspeed" BIGINT
);
CREATE INDEX scraped_popularity ON scraped (("seeders" + "leechers"));

CREATE OR REPLACE FUNCTION tracked_update_scraped() RETURNS trigger AS $$
    DECLARE
        "t_info_hash" BYTEA := CASE TG_OP WHEN 'DELETE' THEN OLD.info_hash ELSE NEW.info_hash END;
        "scraped_info_hash" BYTEA;
        "t_seeders" BIGINT;
        "t_leechers" BIGINT;
        "t_upspeed" BIGINT;
        "t_downspeed" BIGINT;
    BEGIN
        -- Collect data
        SELECT SUM(CASE "left"
                   WHEN 0 THEN 1
                   ELSE 0
                   END),
               SUM(CASE "left"
                   WHEN 0 THEN 0
                   ELSE 1
                   END),
               SUM("upspeed"),
               SUM("downspeed")
          INTO "t_seeders", "t_leechers", "t_upspeed", "t_downspeed"
          FROM tracked
         WHERE "info_hash"="t_info_hash";

        "t_upspeed" := COALESCE("t_upspeed", 0);
        "t_downspeed" := COALESCE("t_downspeed", 0);

        -- Is worth an entry?
        IF "t_leechers" > 0 OR "t_seeders" > 0 THEN
            UPDATE scraped
               SET "seeders"="t_seeders",
                   "leechers"="t_leechers",
                   "upspeed"="t_upspeed",
                   "downspeed"="t_downspeed"  
             WHERE scraped.info_hash="t_info_hash"
             RETURNING scraped.info_hash INTO "scraped_info_hash";

             -- Row didn't exist? Create:
             IF "scraped_info_hash" IS NULL THEN
                INSERT INTO scraped
                            ("info_hash", "seeders", "leechers", "upspeed", "downspeed")
                     VALUES (t_info_hash, t_seeders, t_leechers, t_upspeed, t_downspeed);
             END IF;
        ELSE
            -- Discard on idle
            DELETE FROM scraped
                  WHERE "info_hash"="t_info_hash";
        END IF;

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER tracked_update_scraped AFTER INSERT OR UPDATE OR DELETE ON tracked
       FOR EACH ROW EXECUTE PROCEDURE tracked_update_scraped();


CREATE TABLE downloads_cache(
       -- Enclosure key:
       "feed" TEXT,
       "item" TEXT,
       "enclosure" TEXT,
       PRIMARY KEY ("feed", "item", "enclosure"),
       FOREIGN KEY ("feed", "item", "enclosure") REFERENCES "enclosures" ("feed", "item", "url"),
       -- Torrent data:
       "info_hash" BYTEA,
       -- Item data:
       "title" TEXT,
       "published" TIMESTAMP,
       "homepage" TEXT,
       "payment" TEXT,
       "image" TEXT
);

CREATE INDEX downloads_cache_published ON downloads_cache ("published");
CREATE INDEX downloads_cache_feed_published ON downloads_cache ("feed", "published");


CREATE OR REPLACE FUNCTION update_downloads_cache(
       t_feed TEXT,
       t_item TEXT,
       t_enclosure TEXT,
       info_hash BYTEA
) RETURNS void AS $$
    DECLARE
       item_rec RECORD;
    BEGIN
        DELETE FROM downloads_cache
              WHERE feed=t_feed
                AND item=t_item
                AND enclosure=t_enclosure;

        SELECT * INTO item_rec
          FROM feed_items
         WHERE feed=t_feed AND id=t_item;

        INSERT INTO downloads_cache
               (feed, item, enclosure,
                info_hash,
                title, published, homepage, payment, image)
               VALUES (t_feed, t_item, t_enclosure,
                       info_hash,
                       item_rec.title, item_rec.published, item_rec.homepage, item_rec.payment, item_rec.image);
    END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_downloads_cache_on_enclosure_torrents(
) RETURNS trigger AS $$
    DECLARE
       t_info_hash BYTEA := NEW.info_hash;
       enclosure RECORD;
    BEGIN
        FOR enclosure IN SELECT *
                           FROM item_torrents
                          WHERE info_hash=t_info_hash
        LOOP
            PERFORM update_downloads_cache(enclosure.feed, enclosure.item, enclosure.url,
                                           t_info_hash);
        END LOOP;

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- downloads_cache depends on enclosure_torrents
CREATE TRIGGER enclosure_torrents_update_downloads_cache AFTER INSERT OR UPDATE ON enclosure_torrents
       FOR EACH ROW
       WHEN (LENGTH(NEW.info_hash) = 20)
       EXECUTE PROCEDURE update_downloads_cache_on_enclosure_torrents();


CREATE OR REPLACE FUNCTION update_downloads_cache_on_feed_items(
) RETURNS trigger AS $$
    DECLARE
       enclosure RECORD;
    BEGIN
        FOR enclosure IN SELECT *
                           FROM item_torrents
                          WHERE feed=NEW.feed AND item=NEW.id
        LOOP
            PERFORM update_downloads_cache(enclosure.feed, enclosure.item, enclosure.url,
                                           enclosure.info_hash);
        END LOOP;

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- downloads_cache depends on feed_items
CREATE TRIGGER feed_items_update_downloads_cache AFTER INSERT, UPDATE ON feed_items
       FOR EACH ROW
       EXECUTE PROCEDURE update_downloads_cache_on_feed_items();

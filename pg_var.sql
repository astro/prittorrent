
CREATE TABLE feed_items ("feed" TEXT NOT NULL REFERENCES "feeds" ("url") ON DELETE CASCADE,
                         "id" TEXT NOT NULL,
                         "title" TEXT,
                         "lang" TEXT,
                         "summary" TEXT,
                         "homepage" TEXT,
                         "published" TIMESTAMP NOT NULL,
                         "payment" TEXT,
                         "image" TEXT,
                         "updated" TIMESTAMP,
                         PRIMARY KEY ("feed", "id"));

CREATE INDEX feed_items_published ON feed_items ("published" DESC);

-- Used for inserting items:
CREATE OR REPLACE FUNCTION no_future(
    TIMESTAMP WITH TIME ZONE
) RETURNS TIMESTAMP WITH TIME ZONE
AS $$
    SELECT CASE
               WHEN $1 > NOW() THEN NOW()
               ELSE $1
           END;
$$ LANGUAGE SQL;

CREATE TABLE enclosures ("feed" TEXT NOT NULL,
                         "item" TEXT NOT NULL,
                         "url" TEXT NOT NULL,
                         "type" TEXT,
                         "title" TEXT,
                         PRIMARY KEY ("feed", "item", "url"),
                         FOREIGN KEY ("feed", "item")
                             REFERENCES "feed_items" ("feed", "id")
                             ON DELETE CASCADE);
CREATE INDEX enclosures_url ON enclosures ("url");

-- Materialized distinct content types per feed
CREATE TABLE feed_types (
    "feed" TEXT NOT NULL,
    "type" TEXT,
    PRIMARY KEY ("feed", "type")
);
CREATE OR REPLACE FUNCTION update_feed_type(
    "f_feed" TEXT,
    "f_type" TEXT
) RETURNS void AS $$
    BEGIN
        PERFORM TRUE
          FROM enclosures
         WHERE "feed"=f_feed AND "type"=f_type;

        IF NOT FOUND THEN
            DELETE FROM enclosures
                  WHERE "feed"=f_feed AND "type"=f_type;
        ELSE
            BEGIN
                INSERT INTO feed_types ("feed", "type")
                VALUES (f_feed, f_type);
            EXCEPTION
                WHEN integrity_constraint_violation
                THEN -- ignore
            END;
        END IF;
    END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION enclosures_update_feed_type(
) RETURNS trigger AS $$
    BEGIN
        PERFORM update_feed_type(NEW.feed, NEW.type);
        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER enclosures_update_feed_type
    AFTER INSERT OR UPDATE ON enclosures
    FOR EACH ROW
    EXECUTE PROCEDURE enclosures_update_feed_type();

CREATE OR REPLACE FUNCTION enclosures_update_feed_type_delete(
) RETURNS trigger AS $$
    BEGIN
        PERFORM update_feed_type(OLD.feed, OLD.type);
        RETURN OLD;
    END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER enclosures_update_feed_type_delete
    AFTER INSERT OR UPDATE ON enclosures
    FOR EACH ROW
    EXECUTE PROCEDURE enclosures_update_feed_type_delete();


CREATE INDEX enclosure_torrents_info_hash
       ON enclosure_torrents (info_hash)
       WHERE LENGTH(info_hash) = 20;
CREATE OR REPLACE VIEW enclosures_to_hash AS
       SELECT enclosures.url,
              enclosure_torrents.last_update AS last_update,
              enclosure_torrents.error AS error,
              enclosure_torrents.info_hash
               FROM enclosures
          LEFT JOIN enclosure_torrents
                 ON (enclosures.url=enclosure_torrents.url)
          LEFT JOIN feeds
                 ON (feeds.url=enclosures.feed)
              WHERE feeds.torrentify AND
                    enclosures.url NOT LIKE '%.torrent' AND
                    enclosures.type != 'application/x-bittorrent' AND
                    (enclosure_torrents.info_hash IS NULL OR
                     LENGTH(enclosure_torrents.info_hash)=0)
           ORDER BY last_update NULLS FIRST;

CREATE INDEX enclosure_torrents_info_hash ON enclosure_torrents (info_hash);

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
        IF next_url IS NULL OR next_url.url IS NULL THEN
            RETURN;
        END IF;

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

CREATE VIEW item_torrents AS
       SELECT enclosures.feed, enclosures.item, enclosures.url,
              enclosure_torrents.info_hash
       FROM enclosure_torrents LEFT JOIN enclosures ON (enclosures.url=enclosure_torrents.url)
       WHERE LENGTH(info_hash)=20;


CREATE VIEW feed_errors AS
    SELECT feeds.url,
           MIN(COALESCE(feeds.error, enclosure.error)) AS error
      FROM feeds
 LEFT JOIN (SELECT feed, error
              FROM enclosures
              JOIN enclosure_torrents USING (url)
             WHERE error != ''
           ) AS enclosure ON (feeds.url = enclosure.feed)
     WHERE feeds.error IS NOT NULL
        OR enclosure.error != ''
  GROUP BY feeds.url;

-- Login UI
-- TODO: write reset functions
CREATE TABLE user_tokens (
       "kind" TEXT,
       "user" TEXT,
       "token" BYTEA PRIMARY KEY,
       "created" TIMESTAMP
);

CREATE TABLE user_sessions (
       "user" TEXT NOT NULL REFERENCES users ("name"),
       "sid" BYTEA PRIMARY KEY,
       "updated" TIMESTAMP
);

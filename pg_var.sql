
CREATE TABLE feed_items ("feed" TEXT NOT NULL REFERENCES "feeds" ("url") ON DELETE CASCADE,
                         "id" TEXT NOT NULL,
                         "title" TEXT,
                         "homepage" TEXT,
                         "published" TIMESTAMP NOT NULL,
                         "payment" TEXT,
                         "image" TEXT,
                         "updated" TIMESTAMP,
                         "xml" TEXT,
                         PRIMARY KEY ("feed", "id"));

CREATE INDEX feed_items_published ON feed_items ("published" DESC);

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
                         FOREIGN KEY ("feed", "item")
                             REFERENCES "feed_items" ("feed", "id")
                             ON DELETE CASCADE);
CREATE INDEX enclosures_url ON enclosures ("url");



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

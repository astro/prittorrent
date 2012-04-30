--
-- downloads_cache
--

CREATE TABLE downloads_cache(
       -- Enclosure key:
       "feed" TEXT,
       "item" TEXT,
       "enclosure" TEXT,
       PRIMARY KEY ("feed", "item", "enclosure"),
       FOREIGN KEY ("feed", "item", "enclosure")
           REFERENCES "enclosures" ("feed", "item", "url")
           ON DELETE CASCADE,
       -- Torrent data:
       "info_hash" BYTEA,
       "name" TEXT,
       "size" BIGINT,
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
       t_info_hash BYTEA
) RETURNS void AS $$
    DECLARE
       item_rec RECORD;
       t_name TEXT;
       t_size BIGINT;
    BEGIN
        DELETE FROM downloads_cache
              WHERE feed=t_feed
                AND item=t_item
                AND enclosure=t_enclosure;

        SELECT * INTO item_rec
          FROM feed_items
         WHERE feed=t_feed AND id=t_item;
        SELECT "name", "size" INTO t_name, t_size
          FROM torrents
          WHERE info_hash=t_info_hash;

        INSERT INTO downloads_cache
               (feed, item, enclosure,
                info_hash, "name", "size",
                title, published, homepage, payment, image)
               VALUES (t_feed, t_item, t_enclosure,
                       t_info_hash, t_name, t_size,
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
CREATE TRIGGER feed_items_update_downloads_cache AFTER INSERT OR UPDATE ON feed_items
       FOR EACH ROW
       EXECUTE PROCEDURE update_downloads_cache_on_feed_items();

-- Because enclosures get deleted and reinserted, not updated
CREATE OR REPLACE FUNCTION update_downloads_cache_on_enclosures(
) RETURNS trigger AS $$
    DECLARE
        t_info_hash BYTEA;
    BEGIN
        SELECT info_hash INTO t_info_hash
          FROM enclosure_torrents
         WHERE url=NEW.url;

        IF t_info_hash IS NOT NULL THEN
            PERFORM update_downloads_cache(NEW.feed, NEW.item, NEW.url,
                                           t_info_hash);
        END IF;
        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- downloads_cache depends on enclosures
CREATE TRIGGER enclosures_update_downloads_cache AFTER INSERT OR UPDATE ON enclosures
       FOR EACH ROW
       EXECUTE PROCEDURE update_downloads_cache_on_enclosures();

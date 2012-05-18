--
-- downloads_cache
--

CREATE TABLE downloads_cache(
       -- User key:
       "user" TEXT,
       "slug" TEXT,
       -- Enclosure key:
       "feed" TEXT,
       "item" TEXT,
       "enclosure" TEXT,
       PRIMARY KEY ("user", "slug", "feed", "item", "enclosure"),
       FOREIGN KEY ("user", "slug", "feed")
            REFERENCES "user_feeds" ("user", "slug", "feed")
            ON DELETE CASCADE,
       FOREIGN KEY ("feed", "item", "enclosure")
           REFERENCES "enclosures" ("feed", "item", "url")
           ON DELETE CASCADE,
       -- Feed data
       "feed_title" TEXT,
       "feed_public" TEXT,
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
       t_user TEXT,
       t_slug TEXT,
       t_feed TEXT,
       t_item TEXT,
       t_enclosure TEXT,
       t_info_hash BYTEA
) RETURNS void AS $$
    DECLARE
       feed_rec RECORD;
       user_feed_rec RECORD;
       item_rec RECORD;
       t_name TEXT;
       t_size BIGINT;
    BEGIN
        DELETE FROM downloads_cache
              WHERE "user"=t_user
                AND "slug"=t_slug
                AND "feed"=t_feed
                AND "item"=t_item
                AND "enclosure"=t_enclosure;

        SELECT * INTO feed_rec
          FROM feeds
         WHERE url=t_feed;
        SELECT * INTO user_feed_rec
          FROM user_feeds
         WHERE "user"=t_user AND "slug"=t_slug AND "feed"=t_feed;
        SELECT * INTO item_rec
          FROM feed_items
         WHERE feed=t_feed AND id=t_item;
        SELECT "name", "size" INTO t_name, t_size
          FROM torrents
          WHERE info_hash=t_info_hash;

        INSERT INTO downloads_cache
               ("user", slug, feed, item, enclosure,
                info_hash, "name", "size",
                feed_title, feed_public,
                title, published, homepage, payment, image)
               VALUES (t_user, t_slug, t_feed, t_item, t_enclosure,
                       t_info_hash, t_name, t_size,
                       COALESCE(user_feed_rec.title, feed_rec.title), user_feed_rec."public",
                       item_rec.title, item_rec.published, item_rec.homepage, item_rec.payment, item_rec.image);
    END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_downloads_cache(
       t_feed TEXT,
       t_item TEXT,
       t_enclosure TEXT,
       t_info_hash BYTEA
) RETURNS void AS $$
    DECLARE
       user_feed RECORD;
    BEGIN
        FOR user_feed IN SELECT *
                           FROM user_feeds
                          WHERE feed=t_feed
        LOOP
            PERFORM update_downloads_cache(user_feed."user", user_feed."slug",
                                           user_feed."feed",
                                           t_item, t_enclosure, t_info_hash);
        END LOOP;
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
CREATE TRIGGER feeds_update_downloads_cache AFTER INSERT OR UPDATE ON feeds
       FOR EACH ROW
       EXECUTE PROCEDURE update_downloads_cache_on_feeds();

CREATE OR REPLACE FUNCTION update_downloads_cache_on_feeds(
) RETURNS trigger AS $$
    DECLARE
       enclosure RECORD;
    BEGIN
        FOR enclosure IN SELECT *
                           FROM item_torrents
                          WHERE feed=NEW.url
        LOOP
            PERFORM update_downloads_cache(enclosure.feed, enclosure.item, enclosure.url,
                                           enclosure.info_hash);
        END LOOP;

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- downloads_cache depends on feeds
CREATE TRIGGER feeds_update_downloads_cache AFTER INSERT OR UPDATE ON feeds
       FOR EACH ROW
       EXECUTE PROCEDURE update_downloads_cache_on_feeds();

CREATE OR REPLACE FUNCTION update_downloads_cache_on_user_feeds(
) RETURNS trigger AS $$
    DECLARE
       enclosure RECORD;
    BEGIN
        FOR enclosure IN SELECT *
                           FROM item_torrents
                          WHERE feed=NEW.feed
        LOOP
            PERFORM update_downloads_cache(enclosure.feed, enclosure.item, enclosure.url,
                                           enclosure.info_hash);
        END LOOP;

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- downloads_cache depends on user_feeds
CREATE TRIGGER user_feeds_update_downloads_cache AFTER INSERT OR UPDATE ON user_feeds
       FOR EACH ROW
       EXECUTE PROCEDURE update_downloads_cache_on_user_feeds();

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


CREATE OR REPLACE VIEW downloads_scraped AS
       SELECT downloads_cache."user", downloads_cache."slug",
              downloads_cache."feed", downloads_cache."item", downloads_cache."enclosure",
              downloads_cache."info_hash", downloads_cache."name", downloads_cache."size",
              downloads_cache."feed_title", downloads_cache."feed_public",
              downloads_cache."title", downloads_cache."published", downloads_cache."homepage", downloads_cache."payment", downloads_cache."image",
              COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
              COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
              COALESCE(scraped.downloaded, 0) AS "downloaded"
         FROM downloads_cache LEFT JOIN scraped ON (downloads_cache.info_hash=scraped.info_hash);

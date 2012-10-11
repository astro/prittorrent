-- TODO: search users

-- search feeds
ALTER TABLE feeds ADD COLUMN search tsvector;
CREATE INDEX search_feeds ON feeds USING gist(search);

CREATE OR REPLACE FUNCTION search_feeds_trigger() RETURNS trigger AS $$
DECLARE
  conf regconfig :=
    CASE NEW.lang
      WHEN 'dk' THEN 'danish'
      WHEN 'nl' THEN 'dutch'
      WHEN 'fi' THEN 'finnish'
      WHEN 'fr' THEN 'french'
      WHEN 'de' THEN 'german'
      WHEN 'hu' THEN 'hungarian'
      WHEN 'it' THEN 'italian'
      WHEN 'no' THEN 'norwegian'
      WHEN 'pt' THEN 'portuguese'
      WHEN 'ro' THEN 'romanian'
      WHEN 'ru' THEN 'russian'
      WHEN 'es' THEN 'spanish'
      WHEN 'sw' THEN 'swedish'
      WHEN 'tr' THEN 'turkish'
      ELSE 'english'
      END;
BEGIN
  NEW.search :=
     setweight(to_tsvector(conf, coalesce(new.title,'')), 'A') ||
     setweight(to_tsvector(conf, coalesce(new.summary,'')), 'B') ||
     setweight(to_tsvector(conf, coalesce(new.url,'')), 'D') ||
     setweight(to_tsvector(conf, coalesce(new.homepage,'')), 'D');
  RETURN NEW;
END
$$ LANGUAGE plpgsql;

CREATE TRIGGER search_feeds_trigger BEFORE INSERT OR UPDATE
    ON feeds FOR EACH ROW EXECUTE PROCEDURE search_feeds_trigger();


CREATE OR REPLACE FUNCTION search_feeds(needle TEXT) RETURNS SETOF feeds AS $$
    DECLARE
        "query" TSQUERY := plainto_tsquery(needle);
    BEGIN
        RETURN QUERY
            SELECT *
              FROM feeds
             WHERE "search" @@ "query"
          ORDER BY ts_rank("search", "query") DESC;
    END;
$$ LANGUAGE plpgsql;

-- search items
ALTER TABLE feed_items ADD COLUMN search tsvector;
CREATE INDEX search_feed_items ON feed_items USING gist(search);

CREATE OR REPLACE FUNCTION search_feed_items_trigger() RETURNS trigger AS $$
DECLARE
  conf regconfig :=
    CASE NEW.lang
      WHEN 'dk' THEN 'danish'
      WHEN 'nl' THEN 'dutch'
      WHEN 'fi' THEN 'finnish'
      WHEN 'fr' THEN 'french'
      WHEN 'de' THEN 'german'
      WHEN 'hu' THEN 'hungarian'
      WHEN 'it' THEN 'italian'
      WHEN 'no' THEN 'norwegian'
      WHEN 'pt' THEN 'portuguese'
      WHEN 'ro' THEN 'romanian'
      WHEN 'ru' THEN 'russian'
      WHEN 'es' THEN 'spanish'
      WHEN 'sw' THEN 'swedish'
      WHEN 'tr' THEN 'turkish'
      ELSE 'english'
      END;
BEGIN
  NEW.search :=
     setweight(to_tsvector(conf, coalesce(new.title,'')), 'A') ||
     setweight(to_tsvector(conf, coalesce(new.summary,'')), 'B') ||
     setweight(to_tsvector(conf, coalesce(new.homepage,'')), 'D');
  RETURN NEW;
END
$$ LANGUAGE plpgsql;

CREATE TRIGGER search_feed_items_trigger BEFORE INSERT OR UPDATE
    ON feed_items FOR EACH ROW EXECUTE PROCEDURE search_feed_items_trigger();


CREATE OR REPLACE FUNCTION search_feed_items(
    "limit" INT, "offset" INT, needle TEXT
) RETURNS SETOF download AS $$
    DECLARE
        "query" TSQUERY := plainto_tsquery(needle);
    BEGIN
        RETURN QUERY
            SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
                   enclosures.item, enclosures.url AS enclosure,
                   COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
                   torrents.info_hash, torrents.name, torrents.size, enclosures.type,
                   feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
                   COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
                   COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
                   COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
              FROM (SELECT * FROM feed_items
                     WHERE "search" @@ "query"
                  ORDER BY ts_rank(feed_items."search", "query") DESC
                     LIMIT "limit" OFFSET "offset"
                   ) AS feed_items
     JOIN feeds ON (feed_items.feed=feeds.url)
              JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
              JOIN enclosures ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
              JOIN enclosure_torrents ON (enclosure_torrents.url=enclosures.url)
              JOIN torrents USING (info_hash)
         LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash)
         LEFT JOIN downloaded_stats ON (enclosure_torrents.info_hash=downloaded_stats.info_hash)
             WHERE user_feeds."public";
    END;
$$ LANGUAGE plpgsql;

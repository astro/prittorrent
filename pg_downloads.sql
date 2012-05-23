-- model_enclosures:purge/3
CREATE OR REPLACE FUNCTION purge_download(
    "d_user" TEXT,
    "d_slug" TEXT,
    "d_name" TEXT
) RETURNS void AS $$
    DECLARE
        "d_enclosure" TEXT;
    BEGIN
        SELECT "url" INTO d_enclosure
          FROM enclosure_torrents
          JOIN torrents USING (info_hash)
          JOIN enclosures USING (url)
          JOIN user_feeds USING (feed)
         WHERE "user"=d_user AND "slug"=d_slug AND "name"=d_name;
        DELETE FROM enclosure_torrents WHERE "url"=d_enclosure;
    END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE VIEW downloads_by_published AS
    SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
           enclosures.item, enclosures.url AS enclosure,
           feeds.title AS feed_title, user_feeds."public" AS feed_public,
           torrents.info_hash, torrents.name, torrents.size,
           feed_items.title, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
           COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
           COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
           COALESCE(scraped.downloaded, 0) AS "downloaded"
      FROM (SELECT feed, id, title, published, homepage, payment, image
            FROM feed_items
            ORDER BY published DESC
           ) AS feed_items
     JOIN enclosures ON (feed_items.feed=enclosures.feed AND feed_items.id=enclosures.item)
     JOIN enclosure_torrents ON (enclosures.url=enclosure_torrents.url)
     JOIN torrents ON (enclosure_torrents.info_hash=torrents.info_hash)
     JOIN feeds ON (feed_items.feed=feeds.url)
     JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash);


CREATE OR REPLACE VIEW downloads_by_popularity AS
    SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
           enclosures.item, enclosures.url AS enclosure,
           feeds.title AS feed_title, user_feeds."public" AS feed_public,
           torrents.info_hash, torrents.name, torrents.size,
           feed_items.title, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
           COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
           COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
           COALESCE(scraped.downloaded, 0) AS "downloaded"
      FROM (SELECT info_hash, seeders, leechers, upspeed, downspeed, downloaded
              FROM scraped
             ORDER BY (seeders + leechers) DESC, downloaded DESC
           ) AS scraped
      JOIN torrents USING (info_hash)
      JOIN enclosure_torrents ON (scraped.info_hash=enclosure_torrents.info_hash AND LENGTH(enclosure_torrents.info_hash)=20)
      JOIN enclosures USING (url)
      JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
      JOIN feeds ON (feed_items.feed=feeds.url)
      JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
     WHERE user_feeds.feed IS NOT NULL;



CREATE OR REPLACE VIEW downloads_by_user AS
    SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
           enclosures.item, enclosures.url AS enclosure,
           feeds.title AS feed_title, user_feeds."public" AS feed_public,
           torrents.info_hash, torrents.name, torrents.size,
           feed_items.title, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
           COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
           COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
           COALESCE(scraped.downloaded, 0) AS "downloaded"
      FROM (SELECT feed, "user", slug, "public"
            FROM user_feeds
           ) AS user_feeds
      JOIN feeds ON (user_feeds.feed=feeds.url)
      JOIN feed_items ON (feeds.url=feed_items.feed)
      JOIN enclosures ON (feed_items.feed=enclosures.feed AND feed_items.id=enclosures.item)
      JOIN enclosure_torrents ON (enclosures.url=enclosure_torrents.url)
      JOIN torrents ON (enclosure_torrents.info_hash=torrents.info_hash)
 LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash);

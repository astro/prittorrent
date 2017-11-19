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


CREATE TYPE download AS (
    "user" TEXT,
    "slug" TEXT,
    "feed" TEXT,
    "item" TEXT,
    "enclosure" TEXT,
    "feed_title" TEXT,
    "feed_public" BOOL,
    "info_hash" BYTEA,
    "name" TEXT,
    "size" BIGINT,
    "type" TEXT,
    "title" TEXT,
    "lang" TEXT,
    "summary" TEXT,
    "published" TIMESTAMP,
    "homepage" TEXT,
    "payment" TEXT,
    "image" TEXT,
    "seeders" INT,
    "leechers" INT,
    "upspeed" BIGINT,
    "downspeed" BIGINT,
    "downloaded" BIGINT
);

CREATE OR REPLACE FUNCTION get_popular_downloads(
    INT, INT
) RETURNS SETOF download AS $$
    SELECT *
      FROM (SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
                   enclosures.item, enclosures.url AS enclosure,
                   COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
                   torrents.info_hash, torrents.name, torrents.size, enclosures.type,
                   feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
                   COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
                   COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
                   COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
              FROM (SELECT info_hash, seeders, leechers, upspeed, downspeed
                      FROM scraped
                     ORDER BY (seeders + leechers) DESC
                     LIMIT $1 OFFSET $2
                   ) AS scraped
              JOIN torrents USING (info_hash)
              JOIN downloaded_stats USING (info_hash)
              JOIN enclosure_torrents ON (scraped.info_hash=enclosure_torrents.info_hash AND LENGTH(enclosure_torrents.info_hash)=20)
              JOIN enclosures USING (url)
              JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
              JOIN feeds ON (feed_items.feed=feeds.url)
              JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
             WHERE user_feeds."public"
           ) AS s
    ORDER BY (seeders + leechers) DESC, downloaded DESC;
$$ LANGUAGE SQL;
 

CREATE OR REPLACE FUNCTION get_most_downloaded(
    INT, INT, INT
) RETURNS SETOF download AS $$
    SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
           enclosures.item, enclosures.url AS enclosure,
           COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
           torrents.info_hash, torrents.name, torrents.size, enclosures.type,
           feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
           COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
           COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
           COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
      FROM (SELECT info_hash, downloaded
              FROM downloaded_stats
          ORDER BY (CASE WHEN $3 <= 1 THEN downloaded1
                         WHEN $3 <= 7 THEN downloaded7
                         WHEN $3 <= 30 THEN downloaded30
                         ELSE downloaded
                    END) DESC
             LIMIT $1 OFFSET $2
           ) AS downloaded_stats
      JOIN torrents USING (info_hash)
      JOIN enclosure_torrents USING (info_hash)
      JOIN enclosures USING (url)
      JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
      JOIN feeds ON (feed_items.feed=feeds.url)
      JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
 LEFT JOIN scraped USING (info_hash)
     WHERE user_feeds."public";
$$ LANGUAGE SQL;

 
CREATE OR REPLACE FUNCTION get_popular_downloads(
    INT, INT
) RETURNS SETOF download AS $$
    SELECT *
      FROM (SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
                   enclosures.item, enclosures.url AS enclosure,
                   COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
                   torrents.info_hash, torrents.name, torrents.size, enclosures.type,
                   feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
                   COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
                   COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
                   COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
              FROM (SELECT info_hash, seeders, leechers, upspeed, downspeed
                      FROM scraped
                     ORDER BY (seeders + leechers) DESC
                     LIMIT $1 OFFSET $2
                   ) AS scraped
              JOIN torrents USING (info_hash)
              JOIN downloaded_stats USING (info_hash)
              JOIN enclosure_torrents ON (scraped.info_hash=enclosure_torrents.info_hash AND LENGTH(enclosure_torrents.info_hash)=20)
              JOIN enclosures USING (url)
              JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
              JOIN feeds ON (feed_items.feed=feeds.url)
              JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
             WHERE user_feeds."public"
           ) AS s
    ORDER BY (seeders + leechers) DESC, downloaded DESC;
$$ LANGUAGE SQL;
 

CREATE OR REPLACE FUNCTION get_recent_downloads(
    INT, INT
) RETURNS SETOF download AS $$
    SELECT *
      FROM (SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
                   enclosures.item, enclosures.url AS enclosure,
                   COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
                   torrents.info_hash, torrents.name, torrents.size, enclosures.type,
                   feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
                   COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
                   COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
                   COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
              FROM (SELECT feed, id, title, lang, summary, published, homepage, payment, image
                    FROM feed_items
                    ORDER BY published DESC
                    LIMIT $1 OFFSET $2
                   ) AS feed_items
             JOIN enclosures ON (feed_items.feed=enclosures.feed AND feed_items.id=enclosures.item)
             JOIN enclosure_torrents ON (enclosures.url=enclosure_torrents.url)
             JOIN torrents ON (enclosure_torrents.info_hash=torrents.info_hash)
             JOIN feeds ON (feed_items.feed=feeds.url)
             JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
        LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash)
        LEFT JOIN downloaded_stats ON (enclosure_torrents.info_hash=downloaded_stats.info_hash)
            WHERE user_feeds."public"
      ) AS s
    ORDER BY published DESC;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_recent_downloads(
    INT, INT, TEXT
) RETURNS SETOF download AS $$
    SELECT *
      FROM (SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
                   enclosures.item, enclosures.url AS enclosure,
                   COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
                   torrents.info_hash, torrents.name, torrents.size, enclosures.type,
                   feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
                   COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
                   COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
                   COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
              FROM (SELECT feed, id, title, lang, summary, published, homepage, payment, image
                    FROM feed_items
                    WHERE feed=$3
                    ORDER BY published DESC
                    LIMIT $1 OFFSET $2
                   ) AS feed_items
             JOIN enclosures ON (feed_items.feed=enclosures.feed AND feed_items.id=enclosures.item)
             JOIN enclosure_torrents ON (enclosures.url=enclosure_torrents.url)
             JOIN torrents ON (enclosure_torrents.info_hash=torrents.info_hash)
             JOIN feeds ON (feed_items.feed=feeds.url)
             JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
        LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash)
        LEFT JOIN downloaded_stats ON (enclosure_torrents.info_hash=downloaded_stats.info_hash)
      ) AS s
    ORDER BY published DESC;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_user_recent_downloads(
    INT, INT, TEXT
) RETURNS SETOF download AS $$
    SELECT *
      FROM (SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
                   enclosures.item, enclosures.url AS enclosure,
                   COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
                   torrents.info_hash, torrents.name, torrents.size, enclosures.type,
                   feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
                   COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
                   COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
                   COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
              FROM (SELECT feed, id, title, lang, summary, published, homepage, payment, image
                    FROM feed_items
                    WHERE feed IN (SELECT feed FROM user_feeds WHERE "user"=$3)
                    ORDER BY published DESC
                    LIMIT $1 OFFSET $2
                   ) AS feed_items
             JOIN enclosures ON (feed_items.feed=enclosures.feed AND feed_items.id=enclosures.item)
             JOIN enclosure_torrents ON (enclosures.url=enclosure_torrents.url)
             JOIN torrents ON (enclosure_torrents.info_hash=torrents.info_hash)
             JOIN feeds ON (feed_items.feed=feeds.url)
             JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
        LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash)
        LEFT JOIN downloaded_stats ON (enclosure_torrents.info_hash=downloaded_stats.info_hash)
            WHERE user_feeds."public"
      ) AS s
    ORDER BY published DESC;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_enclosure_downloads(
    TEXT
) RETURNS SETOF download AS $$
    SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
           enclosures.item, enclosures.url AS enclosure,
           COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
           torrents.info_hash, torrents.name, torrents.size, enclosures.type,
           feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
           COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
           COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
           COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
      FROM (SELECT url, info_hash FROM enclosure_torrents
             WHERE url=$1 AND LENGTH(info_hash)=20
             LIMIT 100) AS enclosure_torrents
      JOIN torrents USING (info_hash)
      JOIN enclosures USING (url)
      JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
      JOIN feeds ON (feed_items.feed=feeds.url)
      JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
 LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash)
 LEFT JOIN downloaded_stats ON (enclosure_torrents.info_hash=downloaded_stats.info_hash);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_guid_downloads(
    TEXT
) RETURNS SETOF download AS $$
    SELECT user_feeds."user", user_feeds."slug", user_feeds."feed",
           enclosures.item, enclosures.url AS enclosure,
           COALESCE(user_feeds.title, feeds.title) AS feed_title, user_feeds."public" AS feed_public,
           torrents.info_hash, torrents.name, torrents.size, enclosures.type,
           feed_items.title, feed_items.lang, feed_items.summary, feed_items.published, feed_items.homepage, feed_items.payment, feed_items.image,
           COALESCE(scraped.seeders, 0) AS "seeders", COALESCE(scraped.leechers, 0) AS "leechers",
           COALESCE(scraped.upspeed, 0) AS "upspeed", COALESCE(scraped.downspeed, 0) AS "downspeed",
           COALESCE(downloaded_stats.downloaded, 0) AS "downloaded"
      FROM (SELECT feed, item, url, type FROM enclosures
             WHERE guid=$1
             LIMIT 100) AS enclosures
      JOIN enclosure_torrents USING (url)
      JOIN torrents USING (info_hash)
      JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
      JOIN feeds ON (feed_items.feed=feeds.url)
      JOIN user_feeds ON (feed_items.feed=user_feeds.feed)
 LEFT JOIN scraped ON (enclosure_torrents.info_hash=scraped.info_hash)
 LEFT JOIN downloaded_stats ON (enclosure_torrents.info_hash=downloaded_stats.info_hash)
     WHERE LENGTH(enclosure_torrents.info_hash)=20;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION get_torrent_guids(
    BYTEA
) RETURNS SETOF TEXT AS $$
    SELECT DISTINCT enclosures.guid AS "guid"
      FROM enclosure_torrents
      JOIN enclosures USING (url)
     WHERE enclosure_torrents.info_hash=$1
       AND enclosures.guid IS NOT NULL
$$ LANGUAGE SQL;

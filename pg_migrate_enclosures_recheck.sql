-- State fields
ALTER TABLE enclosure_torrents ADD COLUMN "length" BIGINT;
ALTER TABLE enclosure_torrents ADD COLUMN etag TEXT;
ALTER TABLE enclosure_torrents ADD COLUMN last_modified TEXT;
-- Prefill for migration
UPDATE enclosure_torrents
   SET "length"=(SELECT "size"
                   FROM torrents
                  WHERE info_hash=enclosure_torrents.info_hash
                );

-- Scheduling
ALTER TABLE enclosure_torrents ADD COLUMN next_recheck TIMESTAMP;
CREATE INDEX enclosure_torrents_next_recheck ON enclosure_torrents (next_recheck ASC NULLS FIRST);
CREATE OR REPLACE FUNCTION enclosure_to_recheck(
       OUT e_url TEXT,
       OUT e_length BIGINT,
       OUT e_etag TEXT,
       OUT e_last_modified TEXT
   ) RETURNS RECORD AS $$
    DECLARE
        "next" RECORD;
        "age" INTERVAL;
        "next_interval" INTERVAL;
    BEGIN
        LOCK "enclosure_torrents" IN SHARE ROW EXCLUSIVE MODE;
        SELECT "url", enclosure_torrents."length", "etag", "last_modified"
          INTO "next"
          FROM enclosure_torrents
         WHERE next_recheck IS NULL
            OR next_recheck <= NOW()
         ORDER BY next_recheck ASC NULLS FIRST
         LIMIT 1;

        IF FOUND THEN
            SELECT NOW() - COALESCE(published, '1970-01-01') INTO "age"
              FROM enclosures
              JOIN feed_items ON (enclosures.feed=feed_items.feed AND enclosures.item=feed_items.id)
             WHERE enclosures.url=next.url
             ORDER BY published DESC
             LIMIT 1;
            next_interval = CASE
                            WHEN age < '6 hours' THEN '2 minutes'
                            WHEN age < '24 hours' THEN '5 minutes'
                            WHEN age < '7 days' THEN '15 minutes'
                            ELSE '7 days'
                            END;
            UPDATE enclosure_torrents
               SET next_recheck = NOW() + next_interval
             WHERE url=next.url;

            e_url = next.url;
            e_length = next.length;
            e_etag = next.etag;
            e_last_modified = next.last_modified;
        END IF;
    END;
$$ LANGUAGE plpgsql;

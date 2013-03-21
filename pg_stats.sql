-- Gauges
--
-- For values that change over time (seeders, leechers)
CREATE TABLE gauges (
       "kind" TEXT NOT NULL,
       "time" TIMESTAMP NOT NULL,
       "info_hash" BYTEA,
       "value" BIGINT DEFAULT 1
);
CREATE INDEX gauges_kind_info_hash_time ON gauges ("kind","info_hash","time");
CREATE INDEX gauges_kind_info_hash_time3600 ON gauges ("kind","info_hash",align_timestamp("time", 3600));
CREATE INDEX gauges_kind_info_hash_time21600 ON gauges ("kind","info_hash",align_timestamp("time", 21600));
CREATE INDEX gauges_kind_info_hash_time86400 ON gauges ("kind","info_hash",align_timestamp("time", 86400));
CREATE INDEX gauges_kind_info_hash_time604800 ON gauges ("kind","info_hash",align_timestamp("time", 604800));

CREATE OR REPLACE FUNCTION set_gauge(
       "e_kind" TEXT,
       "e_info_hash" BYTEA,
       "e_value" BIGINT
) RETURNS void AS $$
    DECLARE
        "prev_value" BIGINT;
    BEGIN
        -- Deduplication:
        SELECT "value" INTO "prev_value"
            FROM gauges
           WHERE "kind"="e_kind"
             AND "info_hash"="e_info_hash"
           ORDER BY "time" DESC
           LIMIT 1;
        IF prev_value IS NULL OR prev_value != e_value THEN
            -- Add new datum:
            INSERT INTO gauges
                ("kind", "time", "info_hash", "value")
                VALUES (e_kind, NOW(), e_info_hash, e_value);
        END IF;
    END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION scraped_set_gauges() RETURNS trigger AS $$
    DECLARE
        s_info_hash BYTEA;
        s_seeders INT;
        s_leechers INT;
    BEGIN
        IF TG_OP != 'DELETE' THEN
            s_info_hash := NEW.info_hash;
            s_seeders := NEW.seeders;
            s_leechers := NEW.leechers;
        ELSE
            s_info_hash := OLD.info_hash;
            s_seeders := 0;
            s_leechers := 0;
        END IF;

        PERFORM set_gauge('seeders', s_info_hash, s_seeders);
        PERFORM set_gauge('leechers', s_info_hash, s_leechers);

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- Obtain up/down deltas when a tracked peer is updated
CREATE TRIGGER scraped_set_gauges AFTER INSERT OR UPDATE OR DELETE ON scraped
       FOR EACH ROW EXECUTE PROCEDURE scraped_set_gauges();


-- Counters
--
-- For adding values (up, down, up_seeder)
CREATE TABLE counters (
       "kind" TEXT NOT NULL,
       "time" TIMESTAMP NOT NULL,
       "info_hash" BYTEA,
       "value" BIGINT NOT NULL
);

CREATE INDEX counters_kind_info_hash_time ON counters ("kind","info_hash","time");
CREATE INDEX counters_kind_info_hash_time3600 ON counters ("kind","info_hash",align_timestamp("time", 3600));
CREATE INDEX counters_kind_info_hash_time21600 ON counters ("kind","info_hash",align_timestamp("time", 21600));
CREATE INDEX counters_kind_info_hash_time86400 ON counters ("kind","info_hash",align_timestamp("time", 86400));
CREATE INDEX counters_kind_info_hash_time604800 ON counters ("kind","info_hash",align_timestamp("time", 604800));

CREATE OR REPLACE FUNCTION add_counter(
       "e_kind" TEXT,
       "e_info_hash" BYTEA,
       "e_value" BIGINT
) RETURNS void AS $$
    DECLARE
        period_length BIGINT := 600;
        period TIMESTAMP := TO_TIMESTAMP(
            FLOOR(
                EXTRACT(EPOCH FROM NOW())
                / period_length)
            * period_length);
    BEGIN
        IF e_value = 0 THEN
            -- Nothing to do
            RETURN;
        END IF;

        UPDATE counters SET "value"="value"+e_value
         WHERE "kind"="e_kind"
           AND "info_hash"="e_info_hash"
           AND "time"="period";
        IF NOT FOUND THEN
            INSERT INTO counters
                ("kind", "time", "info_hash", "value")
                VALUES (e_kind, period, e_info_hash, e_value);
        END IF;
    END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION align_timestamp (
    "ts" TIMESTAMP,
    "interval" INT
) RETURNS TIMESTAMP WITH TIME ZONE AS $$
    SELECT TO_TIMESTAMP(FLOOR(EXTRACT(EPOCH FROM $1) / $2) * $2);
$$ LANGUAGE SQL IMMUTABLE;


CREATE TABLE downloaded_stats (
    "info_hash" BYTEA PRIMARY KEY,
    downloaded BIGINT,
    downloaded30 BIGINT,
    downloaded7 BIGINT,
    downloaded1 BIGINT
);

CREATE INDEX downloaded_stats_downloaded ON downloaded_stats (downloaded DESC);
CREATE INDEX downloaded_stats_downloaded30 ON downloaded_stats (downloaded30 DESC);
CREATE INDEX downloaded_stats_downloaded7 ON downloaded_stats (downloaded7 DESC);
CREATE INDEX downloaded_stats_downloaded1 ON downloaded_stats (downloaded1 DESC);

CREATE OR REPLACE FUNCTION update_downloaded_stats(
    "t_info_hash" BYTEA
) RETURNS void AS $$
    DECLARE
        "t_downloaded" BIGINT;
        "t_downloaded30" BIGINT;
        "t_downloaded7" BIGINT;
        "t_downloaded1" BIGINT;
    BEGIN
        DELETE FROM downloaded_stats WHERE info_hash=t_info_hash;

        SELECT COALESCE(SUM("value"), 0)
          INTO "t_downloaded"
          FROM counters
         WHERE "kind"='complete'
           AND "info_hash"=t_info_hash;
        SELECT COALESCE(SUM("value"), 0)
          INTO "t_downloaded30"
          FROM counters
         WHERE "kind"='complete'
           AND "info_hash"=t_info_hash
           AND "time" > (NOW() - '30 days'::INTERVAL);
        SELECT COALESCE(SUM("value"), 0)
          INTO "t_downloaded7"
          FROM counters
         WHERE "kind"='complete'
           AND "info_hash"=t_info_hash
           AND "time" > (NOW() - '7 days'::INTERVAL);
        SELECT COALESCE(SUM("value"), 0)
          INTO "t_downloaded1"
          FROM counters
         WHERE "kind"='complete'
           AND "info_hash"=t_info_hash
           AND "time" > (NOW() - '1 day'::INTERVAL);

        INSERT INTO downloaded_stats (info_hash, downloaded, downloaded30, downloaded7, downloaded1)
             VALUES (t_info_hash, t_downloaded, t_downloaded30, t_downloaded7, t_downloaded1);
    END;
$$ LANGUAGE plpgsql;


-- Only for trigger when "kind"='complete'
CREATE OR REPLACE FUNCTION counters_update_downloaded_stats() RETURNS trigger AS $$
    BEGIN
        PERFORM update_downloaded_stats(NEW.info_hash);

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- Push "downloaded" count to "scraped" cache
CREATE TRIGGER counters_update_downloaded_stats AFTER INSERT OR UPDATE ON counters
       FOR EACH ROW
       WHEN (NEW.kind = 'complete')
       EXECUTE PROCEDURE counters_update_downloaded_stats();



-- Run periodically to update downloaded_stats.downloaded{30,7,1}
CREATE OR REPLACE FUNCTION update_all_downloaded_stats() RETURNS void AS $$
    DECLARE
        t_info_hash BYTEA;
    BEGIN
        FOR t_info_hash IN
            SELECT DISTINCT "info_hash"
              FROM counters
             WHERE kind = 'complete'
               AND time >= (NOW() - '31 days'::INTERVAL)
               AND time <= (NOW() - '1 day'::INTERVAL)
        LOOP
            PERFORM update_downloaded_stats(t_info_hash);
        END LOOP;
    END;
$$ LANGUAGE plpgsql;

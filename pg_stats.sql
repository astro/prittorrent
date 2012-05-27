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

-- Only for trigger when "kind"='complete'
CREATE OR REPLACE FUNCTION counters_update_scraped_downloaded() RETURNS trigger AS $$
    BEGIN
        PERFORM update_scraped(NEW.info_hash);

        RETURN NEW;
    END;
$$ LANGUAGE plpgsql;

-- Push "downloaded" count to "scraped" cache
CREATE TRIGGER counters_update_downloaded AFTER INSERT OR UPDATE ON counters
       FOR EACH ROW
       WHEN (NEW.kind = 'complete')
       EXECUTE PROCEDURE counters_update_scraped_downloaded();


CREATE OR REPLACE FUNCTION fix_up_down_counters() RETURNS void AS $$
    DECLARE
        stime TIMESTAMP;
        skind TEXT;
        prev_skind TEXT;
        sih BYTEA;
        prev_sih BYTEA;
        svalue BIGINT;
        prev_svalue BIGINT;
        diff BIGINT;
    BEGIN
        FOR stime, skind, sih, svalue IN
            SELECT "time", "kind", "info_hash", "value"
              FROM counters
             WHERE kind='up' OR kind='down'
          ORDER BY "kind", "info_hash", "time" ASC
        LOOP
            IF skind = prev_skind AND sih = prev_sih AND prev_svalue IS NOT NULL THEN
                diff := svalue - prev_svalue;
                IF diff < 0 THEN
                   diff := 0;
                END IF;
                UPDATE counters
                   SET "value"=diff
                 WHERE "time"=stime AND "kind"=skind AND "info_hash"=sih;
            END IF;
        END LOOP;
    END;
$$ LANGUAGE plpgsql;

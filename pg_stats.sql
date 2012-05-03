CREATE TABLE events (
       "kind" TEXT NOT NULL,
       "time" TIMESTAMP NOT NULL,
       "info_hash" BYTEA,
       "value" BIGINT DEFAULT 1
);

CREATE OR REPLACE FUNCTION add_event(
       "e_kind" TEXT,
       "e_info_hash" BYTEA,
       "e_value" BIGINT
) RETURNS void AS $$
    BEGIN
        INSERT INTO events
            ("kind", "time", "info_hash", "value")
            VALUES (e_kind, NOW(), e_info_hash, e_value);
    END;
$$ LANGUAGE plpgsql;

CREATE TABLE traffic (
       "kind" TEXT NOT NULL,
       "time" TIMESTAMP NOT NULL,
       "info_hash" BYTEA,
       "value" BIGINT NOT NULL
);

CREATE OR REPLACE FUNCTION add_traffic(
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
        UPDATE traffic SET "value"="value"+e_value
         WHERE "kind"="e_kind"
           AND "info_hash"="e_info_hash"
           AND "time"="period";
        IF NOT FOUND THEN
            INSERT INTO events
                ("kind", "time", "info_hash", "value")
                VALUES (e_kind, period, e_info_hash, e_value);
        END IF;
    END;
$$ LANGUAGE plpgsql;

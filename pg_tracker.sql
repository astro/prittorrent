-- fkey ensures we only track for known torrents:
CREATE TABLE tracked ("info_hash" BYTEA NOT NULL REFERENCES torrents("info_hash"),
       	     	      "peer_id" BYTEA NOT NULL,
		      "host" BYTEA NOT NULL,
		      "port" INT NOT NULL,
		      "uploaded" BIGINT,
		      "downloaded" BIGINT,
		      "downspeed" BIGINT,
		      "upspeed" BIGINT,
		      "left" BIGINT,
		      "last_request" TIMESTAMP NOT NULL,
		      PRIMARY KEY ("info_hash", "peer_id")
		     );

-- Not neccessary due to tracked PKEY?
--CREATE INDEX tracked_info_hash ON tracked ("info_hash");

-- For leechers:
CREATE VIEW tracker AS
       SELECT "info_hash", "peer_id", "host", "port"
         FROM tracked
     ORDER BY "left" ASC, "last_request" DESC
        LIMIT 60;

-- For seeders:
CREATE VIEW tracker_leechers AS
       SELECT "info_hash", "peer_id", "host", "port"
         FROM tracked
        WHERE "left">0
     ORDER BY "left" ASC, "last_request" DESC
        LIMIT 60;

CREATE OR REPLACE FUNCTION set_peer(
       "p_info_hash" BYTEA, "p_host" BYTEA, "p_port" INT, "p_peer_id" BYTEA,
       "p_uploaded" BIGINT, "p_downloaded" BIGINT, "p_left" BIGINT,
       OUT "up" BIGINT, OUT "down" BIGINT
    ) RETURNS RECORD AS $$
    DECLARE
        "old" RECORD;
        "old_age" FLOAT;
        "up" BIGINT;
        "down" BIGINT;
        "p_upspeed" BIGINT;
        "p_downspeed" BIGINT;
    BEGIN
        RAISE NOTICE 'set_peer(%, %)', p_info_hash, p_peer_id;
        SELECT * INTO "old" FROM tracked WHERE "info_hash"="p_info_hash" AND "peer_id"="p_peer_id";
        IF "old" IS NULL THEN
            INSERT INTO tracked ("info_hash", "peer_id", "host", "port",
                                 "uploaded", "downloaded", "left", "last_request")
                VALUES ("p_info_hash", "p_peer_id", "p_host", "p_port",
                        "p_uploaded", "p_downloaded", "p_left", now());
        ELSE
            "old_age" := EXTRACT(EPOCH FROM (now() - old.last_request));
	    RAISE NOTICE 'old_age: %', old_age;
            -- Estimate speeds, with sanity checks first:
            IF "old_age" <= 30 * 60 AND
               "p_uploaded" >= old.uploaded AND
               "p_downloaded" >= old.downloaded AND
               "p_left" <= old.left THEN
                "up" := "p_uploaded" - old.uploaded;
                "down" := "p_downloaded" - old.downloaded;
	        RAISE NOTICE 'up: % down: %',"up","down";
                "p_upspeed" := (up / "old_age")::BIGINT;
                "p_downspeed" := (down / "old_age")::BIGINT;
            END IF;

            UPDATE tracked SET "host"="p_host", "port"="p_port",
                               "uploaded"="p_uploaded", "downloaded"="p_downloaded",
                               "left"="p_left", "last_request"=now(),
                               "upspeed"="p_upspeed", "downspeed"="p_downspeed"
             WHERE "info_hash"="p_info_hash" AND "peer_id"="p_peer_id";
        END IF;
    END;
$$ LANGUAGE plpgsql;

-- periodic tracked cleaner
CREATE OR REPLACE FUNCTION clear_peers(maxage INTERVAL) RETURNS void AS $$
    BEGIN
        DELETE FROM tracked WHERE "last_request" <= CURRENT_TIMESTAMP - maxage;
    END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION scrape_tracker(
       t_info_hash BYTEA,
       OUT t_leechers INT,
       OUT t_seeders INT,
       OUT t_downspeed INT,
       OUT t_downloaded INT
) RETURNS record AS $$
    BEGIN
	SELECT COUNT(peer_id)
	  INTO t_seeders
	  FROM tracked
	 WHERE info_hash = t_info_hash
	   AND "left" <= 0;
	SELECT COUNT(peer_id)
	  INTO t_leechers
	  FROM tracked
	 WHERE info_hash = t_info_hash
	   AND "left" > 0;
	SELECT COALESCE(SUM(downspeed), 0)
	  INTO t_downspeed
	  FROM tracked
	 WHERE info_hash = t_info_hash;
	-- TODO:
	t_downloaded := 0;
    END;
$$ LANGUAGE plpgsql;


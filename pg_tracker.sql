-- fkey ensures we only track for known torrents:
CREATE TABLE tracked ("info_hash" BYTEA NOT NULL REFERENCES torrents("info_hash"),
       	     	      "peer_id" BYTEA NOT NULL,
		      "host" BYTEA NOT NULL,
		      "port" INT NOT NULL,
		      "uploaded" BIGINT,
		      "downloaded" BIGINT,
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
       "p_uploaded" BIGINT, "p_downloaded" BIGINT, "p_left" BIGINT
    ) RETURNS void AS $$
    DECLARE
        "old" RECORD;
    BEGIN
        SELECT * INTO "old" FROM tracked WHERE "info_hash"="p_info_hash" AND "peer_id"="p_peer_id";
        IF "old" IS NULL THEN
            INSERT INTO tracked ("info_hash", "peer_id", "host", "port",
                                 "uploaded", "downloaded", "left", "last_request")
                VALUES ("p_info_hash", "p_peer_id", "p_host", "p_port",
                        "p_uploaded", "p_downloaded", "p_left", CURRENT_TIMESTAMP);
        ELSE
            UPDATE tracked SET "host"="p_host", "port"="p_port",
                               "uploaded"="p_uploaded", "downloaded"="p_downloaded",
                               "left"="p_left", "last_request"=CURRENT_TIMESTAMP
             WHERE "info_hash"="p_info_hash" AND "peer_id"="p_peer_id";
	    -- TODO: return deltas
        END IF;
    END;
$$ LANGUAGE plpgsql;

-- TODO: periodic tracked cleaner


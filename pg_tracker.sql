CREATE TABLE tracked ("info_hash" BYTEA NOT NULL,
       	     	      "peer_id" BYTEA NOT NULL,
		      "host" BYTEA NOT NULL,
		      "port" INT NOT NULL,
		      "uploaded" BIGINT,
		      "downloaded" BIGINT,
		      "left" BIGINT,
		      "last_request" TIMESTAMP NOT NULL,
		      PRIMARY KEY ("info_hash", "peer_id"));

-- Not neccessary due to tracked PKEY?
--CREATE INDEX tracked_info_hash ON tracked ("info_hash");

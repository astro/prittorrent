CREATE TABLE users ("name" TEXT NOT NULL,
       	     	    "email" TEXT NOT NULL,
		    "password" TEXT NOT NULL,
		    "activated" BOOLEAN DEFAULT FALSE,
		    PRIMARY KEY ("name"));
CREATE VIEW activated_users AS
       SELECT "name", "email", "password"
       FROM users
       WHERE "activated";

CREATE TABLE feeds ("url" TEXT NOT NULL,
       	     	    "last_update" TIMESTAMP,
       	     	    "etag" TEXT,
		    "last_modified" TEXT,
		    "error" TEXT,
		    "xml" TEXT,
       	     	    PRIMARY KEY ("url"));

CREATE TABLE user_feeds ("user" TEXT NOT NULL REFERENCES "users" ("name"),
       	     		 "feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
			 PRIMARY KEY ("user", "feed"));


CREATE TABLE feed_items ("feed" TEXT NOT NULL REFERENCES "feeds" ("url"),
       	     		 "id" TEXT NOT NULL,
			 "title" TEXT,
			 "published" TIMESTAMP NOT NULL,
			 "xml" TEXT,
			 PRIMARY KEY ("feed", "id"));

-record(feed_item, {feed :: binary(),
		    id :: binary(),
		    title :: (binary() | null),
		    published :: calendar:datetime(),
		    xml :: binary(),
		    enclosures :: [binary()] }).

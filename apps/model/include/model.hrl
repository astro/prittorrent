-record(feed_item, {feed :: binary(),
		    id :: binary(),
		    title :: (binary() | null),
		    published :: calendar:datetime(),
		    homepage :: binary(),
		    payment :: binary(),
		    xml :: binary(),
		    enclosures :: [binary()] }).

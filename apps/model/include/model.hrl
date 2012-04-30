-record(download, {
	  %% Enclosure key:
	  feed :: binary(),
	  item :: binary(),
	  enclosure :: (binary() | null),
	  %% Torrent data:
	  info_hash :: binary(),
	  name :: binary(),
	  size :: integer(),
	  %% Item data:
	  title :: binary(),
	  published :: calendar:datetime(),
	  homepage :: binary(),
	  payment :: binary(),
	  image :: binary()
	 }).

-record(feed_item, {feed :: binary(),
		    id :: binary(),
		    title :: (binary() | null),
		    published :: calendar:datetime(),
		    homepage :: binary(),
		    payment :: binary(),
		    image :: binary(),
		    xml :: binary(),
		    enclosures :: [binary()],
		    downloads :: [#download{}]
		   }).

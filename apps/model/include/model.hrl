-record(download, {
	  %% User key:
	  user :: binary(),
	  slug :: binary(),
	  %% Enclosure key:
	  feed :: binary(),
	  item :: binary(),
	  enclosure :: (binary() | null),
	  %% Torrent data:
	  info_hash :: binary(),
	  name :: binary(),
	  size :: integer(),
	  type :: binary(),
	  %% Item data:
	  feed_title :: (binary() | null),
	  title :: binary(),
	  lang :: (binary() | null),
	  summary :: (binary() | null),
	  published :: calendar:datetime(),
	  homepage :: binary(),
	  payment :: binary(),
	  %% Enclosure info:
	  image :: binary(),
	  %% Scrape data:
	  seeders :: integer(),
	  leechers :: integer(),
	  upspeed :: integer(),
	  downspeed :: integer(),
	  downloaded :: integer()
	 }).

-record(feed_item, {
	  user :: binary(),
	  slug :: binary(),
	  feed :: binary(),
	  id :: binary(),
	  feed_title :: (binary() | null),
	  title :: (binary() | null),
	  lang :: (binary() | null),
	  summary :: (binary() | null),
	  published :: calendar:datetime(),
	  homepage :: binary(),
	  payment :: binary(),
	  image :: binary(),
	  enclosures :: [binary()],
	  downloads :: [#download{}]
	 }).

-- Meta
create table "meta" (
	"key" text not null,
	"value" any,

	primary key("key")
) strict;
insert into "meta"("key", "value") values ("version", 0);

-- Thumbnails
create table "thumbnail" (
	"path" text not null,
	"date_added" text not null,
	"format_ext" text not null,
	"thumbnail" blob not null,

	primary key("path")
) strict;

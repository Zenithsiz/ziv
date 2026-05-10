select "thumbnail"."date_added", "thumbnail"."format_ext", "thumbnail"."thumbnail"
	from "thumbnail"
	where "thumbnail"."path" = :path

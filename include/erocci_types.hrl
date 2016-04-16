-ifndef(erocci_types_hrl).
-define(erocci_types_hrl, 1).

-define(is_filter(X), is_list(X)).
-define(is_creds(X), element(1, X) =:= creds).

-endif.

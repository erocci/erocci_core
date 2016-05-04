-ifndef(erocci_acl_hrl).
-define(erocci_acl_hrl, true).

-type policy() :: allow | deny.

-type op() :: create 
	    | read
	    | update
	    | {action, occi_category:id()}
	    | delete.

-type location() :: binary() | query.

-type identity() :: owner
		  | {user, erocci_creds:user()}
		  | group
		  | {group, erocci_creds:group()}
		  | authenticated.

-type t() :: {policy(), op(), location(), identity()}.

-endif.

version = 1.0

PROJECT = erocci_core
PROJECT_VERSION = $(shell git describe --always --tags 2> /dev/null | sed -e 's/v\(.*\)/\1/' || echo $(version))

DEPS = occi
dep_occi = git git://github.com/erocci/erlang-occi.git master
dep_jsx_commit = 2.8.0

include erlang.mk

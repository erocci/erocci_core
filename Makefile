version = 0.1

PROJECT = erocci_core
PROJECT_VERSION = $(shell git describe --always --tags 2> /dev/null || echo $(version))

DEPS = occi
dep_occi = git git://github.com/erocci/erlang-occi.git master

include erlang.mk

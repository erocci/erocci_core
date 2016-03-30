PROJECT = erocci_core
PROJECT_VERSION = 0.1

DEPS = occi cowboy
dep_cowboy = git git://github.com/extend/cowboy.git 1.0.1
dep_occi = git git://github.com/erocci/erlang-occi.git master

include erlang.mk

PROJECT = erocci_core
VERSION = 0.1

VSN = $(shell $(CURDIR)/version.sh $(VERSION))

DEPS = occi cowboy
dep_cowboy = git git://github.com/extend/cowboy.git 1.0.1
dep_occi = git git://github.com/erocci/erlang-occi.git master

subst = sed -e 's|@VSN[@]|$(VSN)|g'

include erlang.mk

$(PROJECT).d:: src/$(PROJECT).app.src

src/$(PROJECT).app.src: src/$(PROJECT).app.src.in
	$(gen_verbose) $(subst) $< > $@

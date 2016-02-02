PROJECT = erocci_core
VERSION = 0.1

VSN = $(shell $(CURDIR)/version.sh $(VERSION))

DEPS = cowboy uuid uri jiffy erim_xml
dep_cowboy = git git://github.com/extend/cowboy.git 1.0.1
dep_uuid = git git://github.com/avtobiff/erlang-uuid.git v0.4.7
dep_uri = git git://github.com/heroku/uri.git master
dep_jiffy = git git://github.com/davisp/jiffy.git 0.14.3
dep_erim_xml = git git://github.com/lizenn/erim_xml.git master

subst = sed -e 's|@VSN[@]|$(VSN)|g'

include erlang.mk

$(PROJECT).d:: src/$(PROJECT).app.src

src/$(PROJECT).app.src: src/$(PROJECT).app.src.in
	$(gen_verbose) $(subst) $< > $@

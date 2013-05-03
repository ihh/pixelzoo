PEGJS = $(HOME)/node_modules/pegjs/bin/pegjs

all: rdlang.js pzlang.js

%.js: %.pegjs
	$(PEGJS) --track-line-and-column $< $@

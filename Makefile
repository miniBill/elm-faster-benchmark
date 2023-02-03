.PHONY: all
all: dist/index.html dist/favicon.ico

ELM_FILES = $(shell find src -type f -name '*.elm')
ELM = elm # elm-optimize-level-2

dist/favicon.ico:
	touch $@

dist/index.html: src/index.html dist/frontend-js.js dist/frontend-elm.js dist/backend-js.js dist/backend-elm.js
	cp src/index.html dist/index.html

dist/%-js.js: src/%.js
	mkdir -p dist
	cp $^ $@

dist/frontend-elm.js: $(ELM_FILES)
	mkdir -p dist
	$(ELM) make src/Frontend.elm --output=$@

dist/backend-elm.js: $(ELM_FILES)
	mkdir -p dist
	$(ELM) make src/Backend.elm --output=$@

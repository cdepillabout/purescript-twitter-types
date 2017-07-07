.PHONY: build clean docs install publish repl test watch watch-test
all: build

build:
	npm run build

clean:
	npm run clean

docs:
	npm run docs

install:
	npm install

publish:
	npm run publish

repl:
	npm run repl

test:
	npm run test

watch:
	npm run watch

watch-test:
	npm run watch-test

build:
	@cabal build

test: build
	@cabal test

server: build
	@./dist/build/run-server/run-server -p 8000

.PHONY: build test server

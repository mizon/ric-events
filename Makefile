test:
	@cabal build && cabal test

server:
	@./dist/build/run-server/run-server -p 8000

.PHONY: test server

.PHONY: build
build:
	elm make src/Main.elm --optimize --output=main.js

.PHONY: dev
dev:
	elm reactor

.PHONY: format
format:
	elm-format src --yes

.PHONY: check
check: format build

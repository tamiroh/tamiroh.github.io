ELM_VERSION = 0.19.2-0
ELM_FORMAT_VERSION = 0.8.8

ELM = npx --yes elm@$(ELM_VERSION)
ELM_FORMAT = npx --yes elm-format@$(ELM_FORMAT_VERSION)

.PHONY: build
build:
	$(ELM) make src/Main.elm --optimize --output=main.js

.PHONY: dev
dev:
	$(ELM) reactor

.PHONY: format
format:
	$(ELM_FORMAT) src --yes

.PHONY: validate
validate:
	$(ELM_FORMAT) src --validate

.PHONY: check
check: format build

.PHONY: test lint

test:
	@$(MAKE) --no-print-directory -C tests

lint:
	@$(MAKE) --no-print-directory -C lint

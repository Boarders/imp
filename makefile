build:
	cabal build

run-tests:
	cabal run test

lint:
	hlint --no-exit-code src test

format-code:
	@echo -n "[?] Formatting code..."
	@find src test -type f -name "*.hs" | while read fname; do \
	  stylish-haskell -i "$$fname"; \
	done
	@echo -n -e "\33[2K\r"
	@echo "[✓] Formatting complete!"

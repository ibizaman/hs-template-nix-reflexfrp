.PHONY: build-release-backend
.PHONY: run-release-backend
.PHONY: build-backend
.PHONY: run-backend

build-release-backend:
	nix-build --pure -o backend-result -A ghc.backend

run-release-backend:
	./backend-result/bin/backend-exe

build-backend:
	nix-shell --pure --run 'stack build backend'

run-backend:
	nix-shell --pure --run 'stack run backend'

.PHONY: build-release-backend
.PHONY: run-release-backend

build-release-backend:
	nix-build --pure -o backend-result -A ghc.backend

run-release-backend:
	./backend-result/bin/backend-exe

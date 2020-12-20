.PHONY: build-release-backend
.PHONY: run-release-backend
.PHONY: build-backend
.PHONY: run-backend
.PHONY: build-common
.PHONY: build-release-common
.PHONY: hoogle
.PHONY: hoogle-build
.PHONY: hoogle-generate
.PHONY: hoogle-serve
.PHONY: backend-hoogle-build
.PHONY: clean-cabals
.PHONY: clean

build-release-backend: clean-cabals
	nix-build --pure -o backend-result -A ghc.backend

run-release-backend:
	./backend-result/bin/backend-exe

build-backend: clean-cabals
	nix-shell --pure --run 'stack build backend'

run-backend:
	nix-shell --pure --run 'stack run backend'


build-release-common: clean-cabals
	nix-build --pure -o common-result -A ghc.common

build-common: clean-cabals
	nix-shell --pure --run 'stack build common'

backend-hoogle-build:
	nix-shell --pure --run 'stack build --haddock --haddock-deps'

hoogle-build: backend-hoogle-build

hoogle-generate:
	nix-shell --pure --run 'stack hoogle -- generate --quiet --local'

hoogle-serve:
	nix-shell --pure --command 'hoogle serve --local -p 65000 --database=$$(stack path --local-hoogle-root)/database.hoo'

hoogle: hoogle-build hoogle-generate hoogle-server


clean-cabals:
	rm -f common/common.cabal
	rm -f backend/backend.cabal

clean: clean-cabals

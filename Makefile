.PHONY: backend-release-build
.PHONY: backend-release-run
.PHONY: backend-build
.PHONY: backend-run
.PHONY: hoogle-build-backend
.PHONY: common-build
.PHONY: common-release-build
.PHONY: hoogle-build-backend
.PHONY: hoogle-build
.PHONY: hoogle-generate
.PHONY: hoogle-serve
.PHONY: hoogle
.PHONY: clean-cabals
.PHONY: clean

backend-release-build: clean-cabals
	nix-build --pure -o backend-result -A ghc.backend

backend-release-run:
	./backend-result/bin/backend-exe

backend-build: clean-cabals
	nix-shell --pure --run 'stack build backend'

backend-run:
	nix-shell --pure --run 'stack run backend'


common-release-build: clean-cabals
	nix-build --pure -o common-result -A ghc.common

common-build: clean-cabals
	nix-shell --pure --run 'stack build common'

hoogle-build-backend:
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

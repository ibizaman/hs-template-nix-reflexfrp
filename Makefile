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

common/common.cabal: common/package.yaml
	nix-shell --pure --run 'hpack $<'

backend/backend.cabal: backend/package.yaml
	nix-shell --pure --run 'hpack $<'

frontend/frontend.cabal: frontend/package.yaml
	nix-shell --pure --run 'hpack $<'


backend-release-build: backend/backend.cabal common/common.cabal
	nix-build --pure -o backend-result -A ghc.backend

backend-release-run:
	./backend-result/bin/backend-exe

backend-build: backend/backend.cabal common/common.cabal
	nix-shell --pure --run 'cabal build backend'

backend-run:
	nix-shell --pure --run 'cabal run backend-exe'


common-release-build: common/common.cabal
	nix-build --pure -o common-result -A ghc.common

common-build: common/common.cabal
	nix-shell --pure --run 'cabal build common'


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

clean-tmp:
	rm -rf backend-result
	rm -rf common-result

clean: clean-cabals clean-tmp

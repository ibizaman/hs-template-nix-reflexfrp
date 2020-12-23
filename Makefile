.PHONY: backend-release-build
.PHONY: backend-release-run
.PHONY: backend-build
.PHONY: backend-run
.PHONY: hoogle-build-backend
.PHONY: common-build
.PHONY: common-release-build
.PHONY: frontend-release-build
.PHONY: frontend-build
.PHONY: frontend-run
.PHONY: frontend-desktop-release-build
.PHONY: frontend-desktop-build
.PHONY: frontend-desktop-run
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


frontend-release-build: frontend/frontend.cabal common/common.cabal
	nix-build --pure -o frontend-result -A ghcjs.frontend

frontend-release-run:
	@echo "Open in your browser file://$$PWD/frontend-result/bin/frontend-exe.jsexe/index.html"

frontend-build: frontend/frontend.cabal common/common.cabal
	nix-shell --pure --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs build frontend" shell.ghcjs.nix

frontend-run:
	@echo "Open in your browser file://$$PWD/$$(find dist-ghcjs/ -name index.html)"


frontend-warp-release-build: frontend/frontend.cabal common/common.cabal
	nix-build --pure -o frontend-warp-result --arg useWarp true -A ghc.frontend

frontend-warp-release-run:
	./frontend-warp-result/bin/frontend-exe

frontend-warp-build: frontend/frontend.cabal common/common.cabal
	nix-shell --pure --run "cabal build frontend" --arg useWarp true shell.nix

frontend-warp-run:
	nix-shell --pure --run "cabal run frontend" --arg useWarp true shell.nix


frontend-desktop-release-build: frontend/frontend.cabal common/common.cabal
	nix-build --pure -o frontend-desktop-result --arg useWarp false -A ghc.frontend

frontend-desktop-release-run:
	./frontend-desktop-result/bin/frontend-exe

frontend-desktop-build: frontend/frontend.cabal common/common.cabal
	nix-shell --pure --run "cabal build frontend" --arg useWarp false shell.nix

frontend-desktop-run:
	nix-shell --pure --run "cabal run frontend" --arg useWarp false shell.nix


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
	rm -f frontend/frontend.cabal
	rm -f backend/backend.cabal

clean-tmp:
	rm -rf backend-result
	rm -rf common-result
	rm -rf frontend-result
	rm -rf frontend-warp-result
	rm -rf frontend-desktop-result

clean: clean-cabals clean-tmp

.DEFAULT_GOAL := all

## parameters

name              ?= telescheme
namespace         ?= git.backbone/corpix
version           ?= development

PARALLEL_JOBS ?= 8
NIX_OPTS      ?=

## bindings

root                := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
tmux                := tmux -2 -f $(PWD)/.tmux.conf -S $(PWD)/.tmux
tmux_session        := $(name)
nix                 := nix $(NIX_OPTS)
nix_dir             := ./nix
docker_shell_volume := nix
build_dir           := ./build
binary              := $(build_dir)/main

### reusable and long opts for commands inside rules

docker_shell_opts = -v $(docker_shell_volume):/nix:rw \
	-v $(root):/chroot                            \
	-e COLUMNS=$(COLUMNS)                         \
	-e LINES=$(LINES)                             \
	-e TERM=$(TERM)                               \
	-e NIX_BUILD_CORES=$(NIX_BUILD_CORES)         \
	-e HOME=/chroot                               \
	-w /chroot                                    \
	--hostname $(namespace).localhost             \
	$(foreach v,$(ports), -p $(v):$(v) )

wildcard/r = $(foreach d,$(wildcard $1*),$(call wildcard/r,$d/,$2)$(filter $(subst *,%,$2),$d))
tests := $(foreach v,$(call wildcard/r,,*-test.ss),:corpix/gerbilstd/$(patsubst %.ss,%,$(v)))

## macro

define fail
{ echo "error: "$(1) 1>&2; exit 1; }
endef

## targets

.PHONY: all
all: build # test, check and build all cmds

.PHONY: help
help: # print defined targets and their comments
	@grep -Po '^[a-zA-Z%_/\-\s]+:+(\s.*$$|$$)' Makefile \
		| sort                                      \
		| sed 's|:.*#|#|;s|#\s*|#|'                 \
		| column -t -s '#' -o ' | '

### releases

### development

.PHONY: build
build: # build application `binary`
	mkdir -p $(build_dir)
	gxc -exe -static -cc-options '$(CFLAGS)' -ld-options '$(LDFLAGS)' -o $(binary) ./main.scm

.PHONY: run
run: build # run application
	$(binary)

.PHONY: test
test: # run unit tests
	gxi                                              \
		-e "(add-load-path (current-directory))" \
		-e "(import :corpix/gerbilstd/test)"     \
		-e "(import $(tests))"                   \
		-e "(test!)"


#### environment management

.PHONY: run/nix/cage
run/nix/cage: # run nix sandboxed shell
	@exec nix-cage

.PHONY: run/tmux/session
run/tmux/session: # start tmux development environment
	@$(tmux) has-session    -t $(tmux_session) && $(call fail,tmux session socket $(tmux_session) already exists) || true
	@$(tmux) new-session    -s $(tmux_session) -n console -d
	@$(tmux) select-window  -t $(tmux_session):0

	@if [ -f $(root)/.personal.tmux.conf ]; then             \
		$(tmux) source-file $(root)/.personal.tmux.conf; \
	fi

	@exec $(tmux) attach-session -t $(tmux_session)

.PHONY: run/docker/clean
run/docker/clean: # clean docker development environment artifacts
	docker volume rm nix

.PHONY: run/docker/shell
run/docker/shell: # run development environment shell
	@exec docker run --rm -it                     \
		--log-driver=none                     \
		$(docker_shell_opts) nixos/nix:latest \
		nix-shell --command "exec make run/tmux/session"

####

.PHONY: run/swank
run/swank: ~/.gerbil/pkg/github.com/drewc/drewc-r7rs-swank # run swank server for slime
	gxi                                              \
		-e "(import :drewc/gerbil-swank)"        \
		-e "(add-load-path (current-directory))" \
		-e "(start-swank)"

~/.gerbil/pkg/github.com/drewc/drewc-r7rs-swank:
	gxpkg install github.com/drewc/drewc-r7rs-swank

##

.PHONY: clean
clean: # clean stored state
	rm -rf result*

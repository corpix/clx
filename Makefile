.DEFAULT_GOAL := all

name           = clx
namespace      = corpix
version       ?= development

root          := $(patsubst %/,%,$(dir $(realpath $(firstword $(MAKEFILE_LIST)))))
nix_dir       := nix
tmux          := tmux -2 -f $(root)/.tmux.conf -S $(root)/.tmux
tmux_session  := $(name)
nix           := nix $(NIX_OPTS)
sbcl          := sbcl --noinform --load $(root)/.quicklisp/setup.lisp --load $(root)/setup.lisp


shell_volume := nix
shell_opts = -v $(shell_volume):/nix:rw         \
	-v $(root):/dev                         \
	-e COLUMNS=$(COLUMNS)                   \
	-e LINES=$(LINES)                       \
	-e TERM=$(TERM)                         \
	-e NIX_BUILD_CORES=$(NIX_BUILD_CORES)   \
	-e HOME=/dev                            \
	-w /dev                                 \
	--hostname localhost                    \
	$(foreach v,$(ports), -p $(v):$(v) )

## helpers

, = ,

wildcard/r = $(foreach d,$(wildcard $1*),$(call wildcard/r,$d/,$2)$(filter $(subst *,%,$2),$d))

## macro

define fail
{ echo "error: "$(1) 1>&2; exit 1; }
endef

## targets

.PHONY: all
all: init

.PHONY: help
help: # print defined targets and their comments
	@grep -Po '^[a-zA-Z%_/\-\s]+:+(\s.*$$|$$)' Makefile \
		| sort                                      \
		| sed 's|:.*#|#|;s|#\s*|#|'                 \
		| column -t -s '#' -o ' | '

## releases

## development

.quicklisp/setup.lisp: # initialize quicklisp directory
	quicklisp init

.PHONY: init
init: .quicklisp/setup.lisp # initialize project for development

.PHONY: build
build: .quicklisp/setup.lisp build.lisp # build binary
	$(sbcl) --disable-debugger --load build.lisp

.PHONY: run
run: build
	./main

.PHONY: test
test: .quicklisp/setup.lisp # run unit tests
	$(sbcl) --disable-debugger --load test.lisp


##

.PHONY: run/swank
run/swank: .quicklisp/setup.lisp # run swank server for slime
	$(sbcl) --disable-debugger --load $(root)/swank.lisp

.PHONY: run/slynk
run/slynk: .quicklisp/setup.lisp # run slynk server for sly
	$(sbcl) --load $(root)/slynk.lisp


## testing

## runners

test/clickhouse/data: # make sure clickhouse data directory exists
	mkdir -p $@

$(foreach v,$(wildcard test/clickhouse/server*),$(v)/data): # make sure clickhouse cluster data directories exists
	mkdir -p $@

$(foreach v,$(wildcard test/clickhouse/server*),$(v)/config.xml): test/clickhouse/config.cluster.xml # create clickhouse cluster node config
	cp -f $< $@
	@# http, tcp, interserver_http ports
	sed -i 's/_port>\(.\).\(..\)<\//_port>\1$(patsubst test/clickhouse/server%/,%,$(dir $@))\2<\//g' $@
	@# prometheus ports
	sed -i 's/port>800.<\//port>800$(patsubst test/clickhouse/server%/,%,$(dir $@))<\//g' $@

$(foreach v,$(wildcard test/clickhouse/server*),$(v)/users.xml): test/clickhouse/users.xml # create clickhouse cluster node users config
	cp -f $< $@

.PHONY: run/clickhouse
run/clickhouse: test/clickhouse/data # run clickhouse server
	@bash -xec "cd $(dir $<); exec clickhouse server -C config.xml"

.PHONY: run/clickhouse/cluster
run/clickhouse/cluster: $(foreach v,$(wildcard test/clickhouse/server*),$(v)/data $(v)/config.xml $(v)/users.xml) # run clickhouse cluster
	echo -e $(foreach v,$(wildcard test/clickhouse/server*),                               \
		'$(notdir $(v)): bash -xec "cd $(v); exec clickhouse server -C config.xml"\n') \
		| sed 's/^\s*//g'                                                              \
		| hivemind -

.PHONY: run/clickhouse/client
run/clickhouse/client: test/clickhouse/data # run interactive clickhouse client
	@bash -xec "cd $(dir $<); exec clickhouse client --port 9100"

clean:: # remove clickhouse data
	rm -rf test/clickhouse/data || true
	rm -rf test/clickhouse/server*/data || true

##

test/zookeeper/data test/zookeeper/txlog: # make sure zookeeper data directory exists
	mkdir -p $@

$(foreach v,$(wildcard test/zookeeper/server*),$(v)/data/myid): # make sure zookeeper cluster node id exists
	echo $(firstword $(subst /, ,$(patsubst test/zookeeper/server%,%,$@))) > $@

$(foreach v,$(wildcard test/zookeeper/server*),$(v)/data $(v)/txlog): # make sure zookeeper cluster data directories exists
	mkdir -p $@

$(foreach v,$(wildcard test/zookeeper/server*),$(v)/zoo.cfg): test/zookeeper/zoo.cluster.cfg # create zookeeper cluster node config
	cp -f $< $@
	sed -i 's/=2\(.\)8./=2\18$(patsubst test/zookeeper/server%/,%,$(dir $@))/g' $@

$(foreach v,$(wildcard test/zookeeper/server*),$(v)/log4j.properties): test/zookeeper/log4j.properties # create zookeeper cluster node logger configuration
	cp -f $< $@

.PHONY: run/zookeeper
run/zookeeper: test/zookeeper/data test/zookeeper/txlog # run zookeeper server
	@bash -xec "cd $(dir $<); exec zkServer.sh --config . start-foreground"

.PHONY: run/zookeeper/cluster
run/zookeeper/cluster: $(foreach v,$(wildcard test/zookeeper/server*),$(v)/data $(v)/data/myid $(v)/txlog $(v)/zoo.cfg $(v)/log4j.properties) # run zookeeper cluster
	echo -e $(foreach v,$(wildcard test/zookeeper/server*),                                        \
		'$(notdir $(v)): bash -xec "cd $(v); exec zkServer.sh --config . start-foreground"\n') \
		| sed 's/^\s*//g'                                                                      \
		| hivemind -

.PHONY: run/zookeeper/client
run/zookeeper/client: # run zookeeper client
	zkCli.sh -server 127.0.0.1:2181

clean:: # remove zookeeper data
	rm -rf test/zookeeper/data test/zookeeper/txlog || true
	rm -rf test/zookeeper/server*/data test/zookeeper/server*/txlog || true

##

test/prometheus/data: # make sure prometheus data directory exists
	mkdir -p $@

.PHONY: run/prometheus
run/prometheus: test/prometheus/data # run prometheus metrics collection service
	@bash -xec "cd $(dir $<); exec prometheus --config.file=./prometheus.yml --storage.tsdb.path=./data"

clean:: # remove prometheus data
	rm -rf test/prometheus/data

## env

.PHONY: run/shell
run/shell: # enter development environment with nix-shell
	nix-shell

.PHONY: run/cage/shell
run/cage/shell: # enter sandboxed development environment with nix-cage
	nix-cage

.PHONY: run/repl
run/repl: # run sbcl repl
	rlwrap sbcl

.PHONY: run/nix/repl
run/nix/repl: # run nix repl for nixpkgs from env
	nix repl '<nixpkgs>'

## dev session

.PHONY: run/tmux/session
run/tmux/session: # start development environment
	@$(tmux) has-session    -t $(tmux_session) && $(call fail,tmux session $(tmux_session) already exists$(,) use: '$(tmux) attach-session -t $(tmux_session)' to attach) || true
	@$(tmux) new-session    -s $(tmux_session) -n console -d
	@while !$(tmux) select-window  -t $(tmux_session):0; do sleep 0.5; done

	@if [ -f $(root)/.personal.tmux.conf ]; then             \
		$(tmux) source-file $(root)/.personal.tmux.conf; \
	fi

	@$(tmux) attach-session -t $(tmux_session)

.PHONY: run/tmux/attach
run/tmux/attach: # attach to development session if running
	@$(tmux) attach-session -t $(tmux_session)

.PHONY: run/tmux/kill
run/tmux/kill: # kill development environment
	@$(tmux) kill-session -t $(tmux_session)

## docker runners

.PHONY: run/docker/shell
run/docker/shell: # run development environment shell
	@docker run --rm -it                   \
		--log-driver=none              \
		$(shell_opts) nixos/nix:latest \
		nix-shell --run 'exec make run/shell'

.PHONY: run/docker/clean
run/docker/clean: # clean development environment artifacts
	docker volume rm nix

SHELL       := bash
.SHELLFLAGS := -e -o pipefail -c
MAKEFLAGS   += --warn-undefined-variables

all: version generate lint test

build-cli:
	go build ./cmd/char

.PHONY: test
test: version generate lint
	go test -count=1 -cover ./...
	go test -count=1 -race -coverpkg=./... ./...
	go run cmd/char/main.go -timeout 20s cmd/char/testdata/fibtc.char

.PHONY: generate-all
generate-all: generate generate-docs

.PHONY: generate
generate: version
	go generate ./...

.PHONY: lint
lint: version
	staticcheck -checks all,-SA1019,-ST1000 ./...
	go vet ./...

.PHONY: generate-docs
generate-docs: version
	go run ./cmd/chardoc ./stdlib/time ./docs/stdlib-time.md
	go run ./cmd/chardoc ./stdlib/fmt ./docs/stdlib-fmt.md
	go run ./cmd/chardoc ./stdlib/strings ./docs/stdlib-strings.md
	go run ./cmd/chardoc ./stdlib/json ./docs/stdlib-json.md

.PHONY: version
version:
	@go version

.PHONY: clean
clean:
	find . -type f \( -name "cpu.out" -o -name "*.test" -o -name "mem.out" \) -delete
	rm -f cmd/char/char cmd/char/char.exe


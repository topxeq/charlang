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
generate-all: generate

.PHONY: generate
generate: version
	go generate ./...

.PHONY: lint
lint: version
	staticcheck -checks all,-SA1019,-ST1000 ./...
	go vet ./...

.PHONY: version
version:
	@go version

.PHONY: clean
clean:
	find . -type f \( -name "cpu.out" -o -name "*.test" -o -name "mem.out" \) -delete
	rm -f cmd/char/char cmd/char/char.exe


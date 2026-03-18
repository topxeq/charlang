SHELL       := bash
.SHELLFLAGS := -e -o pipefail -c
MAKEFLAGS   += --warn-undefined-variables

# Version information - can be overridden at build time
VERSION ?= $(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")
COMMIT ?= $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
BUILD_DATE ?= $(shell date -u +"%Y-%m-%dT%H:%M:%SZ")

# Build flags with version information
LDFLAGS := -ldflags "-X github.com/topxeq/charlang.VersionG=$(VERSION) -X github.com/topxeq/charlang.CommitG=$(COMMIT) -X github.com/topxeq/charlang.BuildDateG=$(BUILD_DATE)"

all: version generate lint test

# Default target to build with version info
build: build-cli

build-cli:
	cd cmd && go build $(LDFLAGS) -o ../char ./

# Build for multiple platforms
build-all: build-linux build-windows build-darwin build-android

build-linux:
	mkdir -p dist
	cd cmd && GOOS=linux GOARCH=amd64 go build $(LDFLAGS) -o ../dist/char-linux-amd64 ./
	gzip -f dist/char-linux-amd64

build-windows:
	mkdir -p dist
	cd cmd && GOOS=windows GOARCH=amd64 go build $(LDFLAGS) -o ../dist/char-windows-amd64.exe ./
	gzip -f dist/char-windows-amd64.exe
	cd cmd && GOOS=windows GOARCH=amd64 go build $(LDFLAGS) -tags=noGui -o ../dist/charw-windows-amd64.exe ./
	gzip -f dist/charw-windows-amd64.exe

build-darwin:
	mkdir -p dist
	cd cmd && GOOS=darwin GOARCH=amd64 go build $(LDFLAGS) -o ../dist/char-darwin-amd64 ./
	gzip -f dist/char-darwin-amd64
	cd cmd && GOOS=darwin GOARCH=arm64 go build $(LDFLAGS) -o ../dist/char-darwin-arm64 ./
	gzip -f dist/char-darwin-arm64

build-android:
	mkdir -p dist
	cd cmd && GOOS=android GOARCH=arm64 go build $(LDFLAGS) -o ../dist/char-android-arm64 ./
	gzip -f dist/char-android-arm64

.PHONY: test
test: version generate lint
	go test -count=1 -cover ./...
	go test -count=1 -race -coverpkg=./... ./...
	go run $(LDFLAGS) ./cmd/ -timeout 20s cmd/scripts/testAll.char 2>/dev/null || true

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
	go run ./cmd/chardoc ./stdlib/fmt ./docs/stdlib-fmt.md
	go run ./cmd/chardoc ./stdlib/strings ./docs/stdlib-strings.md
	go run ./cmd/chardoc ./stdlib/json ./docs/stdlib-json.md

.PHONY: version
version:
	@echo "Charlang Version: $(VERSION)"
	@echo "Commit: $(COMMIT)"
	@echo "Build Date: $(BUILD_DATE)"
	@echo "Go Version: $(shell go version)"

.PHONY: clean
clean:
	find . -type f \( -name "cpu.out" -o -name "*.test" -o -name "mem.out" \) -delete
	rm -f char char.exe dist/* cmd/char/char cmd/char/char.exe

.PHONY: install
install:
	cd cmd && go install $(LDFLAGS) ./

# Create a release tag
.PHONY: tag
tag:
	@read -p "Enter version (e.g., v1.0.0): " VERSION; \
	git tag -a $$VERSION -m "Release $$VERSION"; \
	echo "Tag $$VERSION created. Push with: git push origin $$VERSION"


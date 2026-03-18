# Charlang Release & Installation Guide

[中文版本](./README_CN.md)

## Overview

This directory contains scripts and tools for:
- Dynamic version generation at build time
- GitHub Release automation
- Self-update functionality
- Installation scripts for first-time users

## Dynamic Versioning

Charlang uses build-time version injection instead of hardcoding versions in the source code.

### How it works

Version information is injected via Go linker flags (`ldflags`) during compilation:

```bash
go build -ldflags "-X github.com/topxeq/charlang.VersionG=1.2.3 -X github.com/topxeq/charlang.CommitG=abc123 -X github.com/topxeq/charlang.BuildDateG=2024-01-01T12:00:00Z"
```

### Version Variables

- `VersionG`: The semantic version (e.g., "v1.2.3" or "2.1.3")
- `CommitG`: Short git commit hash (e.g., "d298bd9")
- `BuildDateG`: ISO 8601 UTC build timestamp (e.g., "2026-03-18T01:40:22Z")

### Building with Make

```bash
# Build with auto-detected version from git
make build

# Build with specific version
VERSION=v1.2.3 make build

# Build for all platforms
make build-all

# Install to GOPATH
make install
```

### Checking Version

```bash
char -version
```

Output:
```
Charlang by TopXeQ V2.1.3
Commit: d298bd9
Built: 2026-03-18T01:40:22Z
```

## GitHub Release Automation

A GitHub Actions workflow is provided at `.github/workflows/release.yaml` that:

1. Triggers on tag push (`v*` pattern) or manual dispatch
2. Builds binaries for multiple platforms:
   - Linux (amd64)
   - Windows (amd64) - console and GUI versions
   - macOS (amd64, arm64)
   - Android (arm64)
3. Creates a GitHub Release with build artifacts
4. Generates release notes automatically

### Creating a Release

```bash
# Create and push a tag
git tag -a v1.2.3 -m "Release v1.2.3"
git push origin v1.2.3
```

Or use the Makefile helper:
```bash
make tag
# Follow the prompts to enter version number
```

## Self-Update Functionality

Charlang already includes self-update functionality via the `-updateChar` flag:

```bash
char -updateChar
```

This will:
1. Check for the latest version at `https://topget.org/pub/charVersion.txt`
2. Compare with the current version
3. Download the appropriate binary for your OS/arch
4. Backup the current executable
5. Replace with the new version

### Update Server

The update server should provide:
- `charVersion.txt`: Plain text file with the latest version number
- `char.gz` / `char.exe.gz` / etc.: Compressed binaries for each platform

## Installation Scripts

### Linux/macOS

```bash
# Download and run the installer
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | bash

# Or with wget
wget -qO- https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | bash
```

The script will:
1. Detect your OS and architecture
2. Get the latest version
3. Download and install to `/usr/local/bin`
4. Add to PATH if needed

### Windows

```powershell
# Download and run the installer
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.ps1" -OutFile "install.ps1"
.\install.ps1
```

Or one-liner:
```powershell
iex (iwr "https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.ps1")
```

The script will:
1. Detect Windows and architecture
2. Get the latest version
3. Download and install to `%LOCALAPPDATA%\Charlang\bin`
4. Add to user PATH if needed

## Manual Installation

### Download Pre-built Binaries

Download the appropriate binary for your platform from the [Releases page](https://github.com/topxeq/charlang/releases).

### Build from Source

```bash
# Clone the repository
git clone https://github.com/topxeq/charlang.git
cd charlang

# Build
make build

# Install
make install
```

## File Structure

```
charlang/
├── .github/
│   └── workflows/
│       └── release.yaml      # GitHub Release automation
├── scripts/
│   ├── README.md              # This file
│   ├── README_CN.md           # Chinese documentation
│   ├── install.sh             # Linux/macOS installer
│   └── install.ps1            # Windows installer
├── Makefile                   # Build targets with versioning
└── charadd.go                 # Version variables (set at build time)
```

## Troubleshooting

### Permission Denied on Linux/macOS

If you get "permission denied" when running the installer:

```bash
# Run with sudo
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | sudo bash
```

Or install to a directory you own:
```bash
mkdir -p ~/bin
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | INSTALL_PATH=~/bin bash
```

### PATH Not Updated on Windows

Restart your terminal or run:
```powershell
$env:Path = [System.Environment]::GetEnvironmentVariable("Path","User") + ";" + [System.Environment]::GetEnvironmentVariable("Path","Machine")
```

## Contributing

See the main [README.md](../README.md) for general contribution guidelines.

For changes to the release/installation system:
1. Test the install scripts locally
2. Verify the GitHub Actions workflow works
3. Update documentation in both English and Chinese

## License

Same license as the main Charlang project.

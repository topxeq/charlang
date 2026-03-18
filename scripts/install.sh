#!/bin/bash

# Charlang Installation Script
# Installs the latest version of Charlang

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
REPO="topxeq/charlang"
GITHUB_API_URL="https://api.github.com/repos/$REPO/releases/latest"
BINARY_NAME="char"
INSTALL_DIR="/usr/local/bin"

# Detect OS and architecture
detect_os() {
    local os=$(uname -s | tr '[:upper:]' '[:lower:]')
    local arch=$(uname -m)

    case $os in
        linux)
            OS="linux"
            ;;
        darwin)
            OS="darwin"
            ;;
        *)
            echo -e "${RED}Unsupported OS: $os${NC}"
            exit 1
            ;;
    esac

    case $arch in
        x86_64|amd64)
            ARCH="amd64"
            ;;
        arm64|aarch64)
            ARCH="arm64"
            ;;
        *)
            echo -e "${RED}Unsupported architecture: $arch${NC}"
            exit 1
            ;;
    esac

    echo -e "${GREEN}Detected: $OS-$ARCH${NC}"
}

# Get latest version
get_latest_version() {
    echo -e "${YELLOW}Checking for latest version...${NC}"

    if command -v curl &> /dev/null; then
        LATEST_VERSION=$(curl -sL "$GITHUB_API_URL" 2>/dev/null | grep -oP '"tag_name":\s*"\K[v0-9.]+' | tr -d 'v"')
    elif command -v wget &> /dev/null; then
        LATEST_VERSION=$(wget -qO- "$GITHUB_API_URL" 2>/dev/null | grep -oP '"tag_name":\s*"\K[v0-9.]+' | tr -d 'v"')
    else
        echo -e "${RED}Either curl or wget is required${NC}"
        exit 1
    fi

    if [ -z "$LATEST_VERSION" ]; then
        echo -e "${YELLOW}GitHub API access failed, using direct download mode${NC}"
        LATEST_VERSION="latest"
    else
        echo -e "${GREEN}Latest version: $LATEST_VERSION${NC}"
    fi
}

# Download and install
download_and_install() {
    local binary_url="https://github.com/$REPO/releases/latest/download/char-$OS-$ARCH.gz"

    echo -e "${YELLOW}Downloading Charlang $LATEST_VERSION...${NC}"

    # Create temp directory
    local temp_dir=$(mktemp -d)
    trap 'rm -rf "$temp_dir"' EXIT

    local compressed_file="$temp_dir/char.gz"

    # Download
    if command -v curl &> /dev/null; then
        curl -sL -o "$compressed_file" "$binary_url"
    elif command -v wget &> /dev/null; then
        wget -q -O "$compressed_file" "$binary_url"
    fi

    # Decompress
    echo -e "${YELLOW}Installing...${NC}"
    gunzip -c "$compressed_file" > "$temp_dir/$BINARY_NAME"
    chmod +x "$temp_dir/$BINARY_NAME"

    # Install to target directory
    if [ -w "$INSTALL_DIR" ]; then
        mv "$temp_dir/$BINARY_NAME" "$INSTALL_DIR/"
    else
        echo -e "${YELLOW}Requesting sudo privileges for installation...${NC}"
        sudo mv "$temp_dir/$BINARY_NAME" "$INSTALL_DIR/"
    fi

    echo -e "${GREEN}Successfully installed Charlang $LATEST_VERSION to $INSTALL_DIR/$BINARY_NAME${NC}"
}

# Check if already installed
check_existing() {
    # If using "latest" mode, skip version check
    if [ "$LATEST_VERSION" = "latest" ]; then
        return
    fi

    if command -v $BINARY_NAME &> /dev/null; then
        local current_version=$($BINARY_NAME -version 2>&1 | head -n1 | grep -oP 'V\K[0-9.]+' || echo "")
        if [ -n "$current_version" ]; then
            echo -e "${YELLOW}Found existing installation: $current_version${NC}"

            if [ "$current_version" = "$LATEST_VERSION" ]; then
                echo -e "${GREEN}Already up to date!${NC}"
                exit 0
            else
                echo -e "${YELLOW}Will upgrade to $LATEST_VERSION${NC}"
            fi
        fi
    fi
}

# Print usage
usage() {
    echo "Charlang Installation Script"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -h, --help      Show this help message"
    echo "  -v, --version   Show version information"
    echo ""
    echo "Examples:"
    echo "  $0              Install latest version"
}

# Main
main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                usage
                exit 0
                ;;
            -v|--version)
                echo "Charlang Installer v1.0.0"
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                usage
                exit 1
                ;;
        esac
        shift
    done

    echo -e "${GREEN}====================================${NC}"
    echo -e "${GREEN}   Charlang Installer${NC}"
    echo -e "${GREEN}====================================${NC}"
    echo ""

    detect_os
    get_latest_version
    check_existing
    download_and_install

    echo ""
    echo -e "${GREEN}====================================${NC}"
    echo -e "${GREEN}Installation complete!${NC}"
    echo -e "${GREEN}====================================${NC}"
    echo ""
    echo "Run 'char -version' to verify installation"
    echo "Run 'char' to start the REPL"
    echo ""
}

main "$@"

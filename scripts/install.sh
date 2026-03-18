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

# Download and install
download_and_install() {
    local binary_url="https://github.com/$REPO/releases/latest/download/char-$OS-$ARCH.gz"
    local LATEST_VERSION="latest"

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
    else
        echo -e "${RED}Either curl or wget is required${NC}"
        exit 1
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

    echo -e "${GREEN}Successfully installed Charlang to $INSTALL_DIR/$BINARY_NAME${NC}"
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

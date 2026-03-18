# Charlang Installation Script for Windows
# Installs the latest version of Charlang

param(
    [switch]$Help,
    [switch]$Version,
    [string]$InstallPath = "$env:LOCALAPPDATA\Charlang\bin"
)

# Colors for output
$ErrorForegroundColor = 'Red'
$WarningForegroundColor = 'Yellow'
$InformationForegroundColor = 'Green'

function Write-Success {
    param([string]$Message)
    Write-Host $Message -ForegroundColor $InformationForegroundColor
}

function Write-Warning {
    param([string]$Message)
    Write-Host $Message -ForegroundColor $WarningForegroundColor
}

function Write-Error {
    param([string]$Message)
    Write-Host $Message -ForegroundColor $ErrorForegroundColor
}

function usage {
    Write-Host "Charlang Installation Script"
    Write-Host ""
    Write-Host "Usage: .\install.ps1 [OPTIONS]"
    Write-Host ""
    Write-Host "Options:"
    Write-Host "  -Help           Show this help message"
    Write-Host "  -Version        Show version information"
    Write-Host "  -InstallPath    Path to install Charlang (default: %LOCALAPPDATA%\Charlang\bin)"
    Write-Host ""
    Write-Host "Examples:"
    Write-Host "  .\install.ps1              Install latest version"
    Write-Host "  .\install.ps1 -InstallPath C:\tools"
}

function Detect-OS {
    $os = "windows"
    $arch = if ([Environment]::Is64BitOperatingSystem) { "amd64" } else { "386" }

    Write-Success "Detected: $os-$arch"
    return @{ OS = $os; Arch = $arch }
}

function Get-LatestVersion {
    Write-Warning "Checking for latest version..."

    try {
        $versionFileUrl = "https://topget.org/pub/charVersion.txt"
        $latestVersion = (Invoke-WebRequest -Uri $versionFileUrl -UseBasicParsing).Content.Trim()

        if ([string]::IsNullOrEmpty($latestVersion)) {
            Write-Error "Failed to get latest version"
            exit 1
        }

        Write-Success "Latest version: $latestVersion"
        return $latestVersion
    } catch {
        Write-Error "Failed to get latest version: $_"
        exit 1
    }
}

function Check-Existing {
    param([string]$LatestVersion)

    try {
        $charPath = Get-Command "char" -ErrorAction SilentlyContinue
        if ($charPath) {
            $currentVersionOutput = & char -version 2>&1 | Select-Object -First 1
            $currentVersion = if ($currentVersionOutput -match 'V([0-9.]+)') { $matches[1] } else { "" }

            if (-not [string]::IsNullOrEmpty($currentVersion)) {
                Write-Warning "Found existing installation: $currentVersion"

                if ($currentVersion -eq $LatestVersion) {
                    Write-Success "Already up to date!"
                    exit 0
                } else {
                    Write-Warning "Will upgrade to $LatestVersion"
                }
            }
        }
    } catch {
        # No existing installation found
    }
}

function Download-And-Install {
    param(
        [string]$OS,
        [string]$Arch,
        [string]$LatestVersion
    )

    $binaryUrl = "https://topget.org/pub/char.exe.gz"
    $binaryUrlGui = "https://topget.org/pub/charw.exe.gz"

    Write-Warning "Downloading Charlang $LatestVersion..."

    # Create temp directory
    $tempDir = Join-Path $env:TEMP "charlang-install-$(Get-Date -Format 'yyyyMMddHHmmss')"
    New-Item -ItemType Directory -Path $tempDir -Force | Out-Null

    try {
        # Download console version
        $compressedFile = Join-Path $tempDir "char.exe.gz"
        Invoke-WebRequest -Uri $binaryUrl -OutFile $compressedFile -UseBasicParsing

        # Download GUI version
        $compressedFileGui = Join-Path $tempDir "charw.exe.gz"
        try {
            Invoke-WebRequest -Uri $binaryUrlGui -OutFile $compressedFileGui -UseBasicParsing
        } catch {
            Write-Warning "GUI version download failed, skipping..."
        }

        # Create install directory
        if (-not (Test-Path $InstallPath)) {
            New-Item -ItemType Directory -Path $InstallPath -Force | Out-Null
        }

        # Decompress using .NET GZipStream
        Write-Warning "Installing..."

        # Console version
        $inputStream = [System.IO.File]::OpenRead($compressedFile)
        $gzipStream = New-Object System.IO.Compression.GZipStream($inputStream, [System.IO.Compression.CompressionMode]::Decompress)
        $outputFile = Join-Path $InstallPath "char.exe"
        $outputStream = [System.IO.File]::Create($outputFile)
        $gzipStream.CopyTo($outputStream)
        $gzipStream.Close()
        $outputStream.Close()
        $inputStream.Close()

        # GUI version if available
        if (Test-Path $compressedFileGui) {
            $inputStreamGui = [System.IO.File]::OpenRead($compressedFileGui)
            $gzipStreamGui = New-Object System.IO.Compression.GZipStream($inputStreamGui, [System.IO.Compression.CompressionMode]::Decompress)
            $outputFileGui = Join-Path $InstallPath "charw.exe"
            $outputStreamGui = [System.IO.File]::Create($outputFileGui)
            $gzipStreamGui.CopyTo($outputStreamGui)
            $gzipStreamGui.Close()
            $outputStreamGui.Close()
            $inputStreamGui.Close()
        }

        Write-Success "Successfully installed Charlang $LatestVersion to $InstallPath"

        # Add to PATH if needed
        $currentPath = [Environment]::GetEnvironmentVariable("Path", "User")
        if ($currentPath -notlike "*$InstallPath*") {
            Write-Warning "Adding $InstallPath to user PATH..."
            [Environment]::SetEnvironmentVariable("Path", "$currentPath;$InstallPath", "User")
            Write-Success "Added to PATH. Please restart your terminal for changes to take effect."
        }

    } finally {
        # Cleanup
        Remove-Item -Path $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

function main {
    if ($Help) {
        usage
        exit 0
    }

    if ($Version) {
        Write-Host "Charlang Installer v1.0.0"
        exit 0
    }

    Write-Host "====================================" -ForegroundColor Cyan
    Write-Host "   Charlang Installer" -ForegroundColor Cyan
    Write-Host "====================================" -ForegroundColor Cyan
    Write-Host ""

    $osInfo = Detect-OS
    $latestVersion = Get-LatestVersion
    Check-Existing -LatestVersion $latestVersion
    Download-And-Install -OS $osInfo.OS -Arch $osInfo.Arch -LatestVersion $latestVersion

    Write-Host ""
    Write-Host "====================================" -ForegroundColor Cyan
    Write-Host "Installation complete!" -ForegroundColor Cyan
    Write-Host "====================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Run 'char -version' to verify installation"
    Write-Host "Run 'char' to start the REPL"
    Write-Host ""
}

main

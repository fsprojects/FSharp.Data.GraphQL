#!/usr/bin/env pwsh

<#
.SYNOPSIS
    Formats F# files that have changed compared to a base branch using Fantomas.

.DESCRIPTION
    This script:
    1. Gets all files that were added, modified or renamed compared to the merge base with the base branch,
       including uncommitted changes in the working tree
    2. Filters for F# files (.fs, .fsi and .fsx) that still exist
    3. Formats them with the local Fantomas tool using the settings from .editorconfig

    Files excluded by .fantomasignore are skipped by Fantomas itself.
    Fantomas is run twice because some constructs only reach a stable layout on the second pass.

.PARAMETER BaseBranch
    The base branch to compare against. Default is 'dev'.

.PARAMETER DryRun
    If specified, shows what files would be formatted without actually formatting them.

.PARAMETER ShowDetails
    If specified, shows detailed output.

.PARAMETER BatchSize
    Number of files to pass to Fantomas in a single invocation. Default is 10.

.EXAMPLE
    ./format-changed-files.ps1

.EXAMPLE
    ./format-changed-files.ps1 -BaseBranch main -DryRun

.EXAMPLE
    ./format-changed-files.ps1 -ShowDetails -BatchSize 5
#>

[CmdletBinding()]
param(
    [Parameter()]
    [string]$BaseBranch = 'dev',

    [Parameter()]
    [switch]$DryRun,

    [Parameter()]
    [switch]$ShowDetails,

    [Parameter()]
    [ValidateRange(1, [int]::MaxValue)]
    [int]$BatchSize = 10
)

$ErrorActionPreference = 'Stop'

function Get-MergeBase {
    param([string]$BaseBranch)

    foreach ($ref in @($BaseBranch, "origin/$BaseBranch")) {
        $mergeBase = & git merge-base HEAD $ref 2>$null
        if ($LASTEXITCODE -eq 0) {
            if ($ShowDetails) { Write-Host "Merge base with '$ref': $mergeBase" -ForegroundColor Gray }
            return $mergeBase
        }
    }

    throw "Could not find a merge base with '$BaseBranch' or 'origin/$BaseBranch'. Fetch the branch or pass -BaseBranch."
}

function Get-ChangedFSharpFiles {
    param([string]$MergeBase)

    # Comparing the merge base with the working tree includes both committed and uncommitted changes
    $changedFiles = @(& git diff --name-only --diff-filter=AMR $MergeBase)
    if ($LASTEXITCODE -ne 0) { throw "git diff failed with exit code $LASTEXITCODE" }

    # Untracked files are not part of git diff output
    $untrackedFiles = @(& git ls-files --others --exclude-standard)
    if ($LASTEXITCODE -ne 0) { throw "git ls-files failed with exit code $LASTEXITCODE" }

    return @($changedFiles + $untrackedFiles |
        Where-Object { $_ -match '\.(fs|fsi|fsx)$' -and (Test-Path -LiteralPath $_) } |
        Sort-Object -Unique)
}

function Invoke-Fantomas {
    param([string[]]$Files)

    $failedFiles = [System.Collections.Generic.List[string]]::new()

    for ($i = 0; $i -lt $Files.Count; $i += $BatchSize) {
        $batch = $Files[$i..([Math]::Min($i + $BatchSize, $Files.Count) - 1)]

        # Fantomas output is not suppressed: on failure it explains which file could not be parsed and why
        & dotnet tool run fantomas @batch
        if ($LASTEXITCODE -ne 0) { $failedFiles.AddRange([string[]]$batch) }
    }

    return $failedFiles
}

# git reports paths relative to the repository root, so all relative paths must be resolved from there
$repositoryRoot = & git rev-parse --show-toplevel 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Host 'Not in a git repository.' -ForegroundColor Red
    exit 1
}

Push-Location $repositoryRoot
try {
    & dotnet tool run fantomas --version *> $null
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Fantomas is not available as a local .NET tool. Run 'dotnet tool restore' first." -ForegroundColor Red
        exit 1
    }

    $mergeBase = Get-MergeBase -BaseBranch $BaseBranch
    $files = Get-ChangedFSharpFiles -MergeBase $mergeBase

    if ($files.Count -eq 0) {
        Write-Host "No changed F# files compared to '$BaseBranch'." -ForegroundColor Green
        exit 0
    }

    Write-Host "Found $($files.Count) changed F# file(s):" -ForegroundColor Cyan
    $files | ForEach-Object { Write-Host "  - $_" -ForegroundColor Gray }

    if ($DryRun) {
        Write-Host 'Dry run: no files were formatted.' -ForegroundColor Yellow
        exit 0
    }

    $failedFiles = @()
    foreach ($pass in 1..2) {
        if ($ShowDetails) { Write-Host "Fantomas pass $pass of 2..." -ForegroundColor Gray }
        $failedFiles = Invoke-Fantomas -Files $files
    }

    if ($failedFiles.Count -gt 0) {
        Write-Host "Fantomas failed for $($failedFiles.Count) file(s):" -ForegroundColor Red
        $failedFiles | Sort-Object -Unique | ForEach-Object { Write-Host "  - $_" -ForegroundColor Red }
        exit 1
    }

    Write-Host "Formatted $($files.Count) file(s)." -ForegroundColor Green
}
catch {
    Write-Host "Script failed: $_" -ForegroundColor Red
    exit 1
}
finally {
    Pop-Location
}

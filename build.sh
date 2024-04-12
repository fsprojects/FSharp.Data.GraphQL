#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet tool restore
dotnet run --project build/Build.fsproj
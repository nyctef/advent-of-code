#!/usr/bin/env bash

set -e

echo "Running hlint --refactor on Haskell files..."

# Run hlint on all .hs files in src, app, and test directories
find src app test -name "*.hs" -type f -exec hlint {} --refactor --refactor-options='--inplace' \;

echo "Linting complete!"

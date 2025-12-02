#!/usr/bin/env bash

set -e

echo "Running hlint on Haskell files..."

# Run hlint on all .hs files in src, app, and test directories
find src app test -name "*.hs" -type f -exec hlint {} +

echo "Linting complete!"

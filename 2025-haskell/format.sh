#!/usr/bin/env bash

set -e

echo "Formatting Haskell files with ormolu..."

# Find all .hs files in src, app, and test directories and format them
find src app test -name "*.hs" -type f -exec ormolu --mode inplace {} +

echo "Formatting complete!"

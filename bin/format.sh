#!/usr/bin/env sh

DEFAULT_PATTERN='**/*.{md,json}'
npx prettier@2.1.2 --write "${1:-$DEFAULT_PATTERN}"

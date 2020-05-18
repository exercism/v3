#!/bin/bash

DEFAULT_PATTERN='**/*.{md,json}'
npx prettier@2.0.4 --write "${1:-$DEFAULT_PATTERN}"
#!/usr/bin/env bash

set -euo pipefail

# PULL_REQUEST_URL=$(jq -r ".pull_request.url" "$GITHUB_EVENT_PATH")
PULL_REQUEST_URL="https://api.github.com/repos/exercism/v3/pulls/1708"

PR_FILES=$(curl --url "${PULL_REQUEST_URL}/files" --header "authorization: Bearer ${GH_TOKEN}")

JQ_OUT=$(jq -c '.[] | select(.status == "added" or .status == "modified") | select(.filename | match("\\.(md|json)$")) | .filename' < "${PR_FILES}")


   # | \
  #xargs npx prettier@2.0.4 --check

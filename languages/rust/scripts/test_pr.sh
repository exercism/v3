#!/usr/bin/env bash

# this script can be used in two ways:
#
# - source it into the current environment, and then use the
#   `test_pr` function
# - run it directly
#
# in either case, it takes a single argument: the PR number

# limitations: this script will break if the PR edits more than 3000 files or
# has more than 250 commits

set -e

function test_pr() {
    local num="$1"

    if [ -z "$num" ]; then
        echo "Usage: test_pr <NUMBER>"
        exit 1
    fi

    for concept in $(
        curl -s -L "https://api.github.com/repos/exercism/v3/pulls/$num/files?per_page=3000" |
        jq -r '.[] | .filename' |
        grep -E '^languages/rust/exercises/concept/\w+' |
        cut -d/ -f5 |
        sort -u
    ); do
        test_concept "$num" "$concept"
    done
}

function test_concept() {
    local num="$1"
    local concept="$2"

    echo "testing $concept in #$num"

    local dir
    dir="$(mktemp --directory)"
    #shellcheck disable=SC2064
    trap "rm -rf $dir" RETURN

    cd "$dir"
    cargo init --lib --name "$concept" .

    last_commit="$(
        curl -s -L https://api.github.com/repos/exercism/v3/pulls/$num/commits |
        jq -r '.[-1] | .sha'
    )"

    curl -s -L "https://raw.githubusercontent.com/exercism/v3/$last_commit/languages/rust/exercises/concept/$concept/.meta/example.rs" > src/lib.rs
    mkdir tests
    curl -s -L "https://raw.githubusercontent.com/exercism/v3/$last_commit/languages/rust/exercises/concept/$concept/tests/$concept.rs" > "tests/$concept.rs"

    cargo check
    cargo test
    cargo test -- --ignored
}

if [ -n "$1" ]; then
    test_pr "$1"
fi

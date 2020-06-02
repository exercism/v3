#!/usr/bin/env bash

set -euo pipefail

LANGUAGES_HOME="languages"
CONCEPT_EXERCISES="exercises/concept"

function main {
  echo $(pwd)
  find ./"${LANGUAGES_HOME}"/* -maxdepth 0 -type d | while read language_dir; do
    language_name=$(basename "${language_dir}")

    if [[ ! -d "${language_dir}/${CONCEPT_EXERCISES}" ]]; then
      echo "${language_name}: 0"
    else
      count=$(find "${language_dir}/${CONCEPT_EXERCISES}"/* -maxdepth 0 -type d | wc -l)
      echo "${language_name}: ${count}"
    fi
  done
}

function installed {
  cmd=$(command -v "${1}")

  [[ -n "${cmd}" ]] && [[ -f "${cmd}" ]]
  return ${?}
}

function die {
  >&2 echo "Fatal: ${@}"
  exit 1
}

# Check for all required dependencies
deps=(find)
for dep in "${deps[@]}"; do
  installed "${dep}" || die "Missing '${dep}'"
done

if [[ ! -d "./${LANGUAGES_HOME}" ]]; then
  die "Please run this in the v3 repository root directory"
fi

main "$@"; exit
#!/usr/bin/env bash

EXIT_CODE=0
LANGUAGE_DIRS=$(find languages -mindepth 1 -maxdepth 1 -type d | sort)

for LANGUAGE_DIR in $LANGUAGE_DIRS; do
    CONFIG_JSON_FILE="$LANGUAGE_DIR/config.json"

    # Skip track if it doesn't have a config.json file
    if [[ ! -f "$CONFIG_JSON_FILE" ]]; then
        continue
    fi

    LANGUAGE=$(jq -r ".language" $CONFIG_JSON_FILE)
    CONCEPTS=$(jq -r ".exercises | .concept[] | .concepts[]" $CONFIG_JSON_FILE | sort -u)

    for CONCEPT in $CONCEPTS; do
        CONCEPT_DIR="$LANGUAGE_DIR/concepts/$CONCEPT"
        CONCEPT_ABOUT_FILE="$CONCEPT_DIR/about.md"
        CONCEPT_LINKS_FILE="$CONCEPT_DIR/links.json"

        if [[ ! -f "$CONCEPT_ABOUT_FILE" ]]; then
            echo "::error::[$LANGUAGE] $CONCEPT_ABOUT_FILE is missing"
            EXIT_CODE=1
        fi

        if [[ ! -f "$CONCEPT_LINKS_FILE" ]]; then
            echo "::error::[$LANGUAGE] $CONCEPT_LINKS_FILE is missing"
            EXIT_CODE=1
        fi
    done
done

exit $EXIT_CODE

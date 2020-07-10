#!/usr/bin/env bash

echo "Scaffolding '$1'"

cp -r ./template ../../exercises/concept/$1

echo "Template '$1' created for JS."
echo "Replace all '---'s with the appropriate data."
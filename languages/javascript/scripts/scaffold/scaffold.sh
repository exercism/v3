#!/usr/bin/env bash

echo "Scaffolding '$1'"

cp -r ./template ../../exercises/concept/$1

echo "Template '$1' created for JS."
cd ../../exercises/concept/$1

sed -i '' "s/---/$1/g" *

mv concept-name.js $1.js
mv concept-name.spec.js $1.js

echo "Final directory:" && ls

echo "Remember to fill out all details in every file!"
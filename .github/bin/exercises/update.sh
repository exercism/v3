#!/usr/bin/env sh

# Update the exercises summary
dotnet run -p .github/bin/exercises

# Format the documents
npx prettier@2.0.4 --write languages/README.md languages/languages.json

# Add the updated language summary files
git add languages/README.md
git add languages/languages.json

# Check if there is nothing to commit (i.e. no changes to the exercises summary)
if [ -z "$(git status --porcelain)" ]; then
    echo "No changes to the exercises summary"
    exit 0
fi

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

echo "Committing updated exercises summary"

# Commit the changes
git add .
git commit -m "[CI] Update exercises summary"
git push

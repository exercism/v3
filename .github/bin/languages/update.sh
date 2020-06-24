#!/usr/bin/env sh

# Update the languages summary
dotnet run -p .github/bin/languages

# Format the documents
npx prettier@2.0.4 --write languages/README.md languages/languages.json

# Add the updated language summary files
git add languages/README.md
git add languages/languages.json

# Check if there is nothing to commit (i.e. no changes to the languages summary)
if [ -z "$(git status --porcelain)" ]; then
    echo "No changes to the languages summary"
    exit 0
fi

# Checkout to new branch
BRANCH="languagesummary-$(date +%Y%m%d%H%M%S)"
git checkout -b "$BRANCH"

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

# Commit and push the changes
git commit -m "[CI] Update languages summary"
git push origin "$BRANCH"

# Create a PR
gh pr create --title "[CI] Update languages summary" --body "This is an _automatically generated_ PR to update the two language summary files."

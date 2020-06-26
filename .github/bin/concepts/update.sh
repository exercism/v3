#!/usr/bin/env sh

# Update the concepts summary
dotnet run -p .github/bin/concepts

# Format the documents
npx prettier@2.0.4 --write reference/README.md reference/references.json

# Add the updated concept summary files
git add reference/README.md
git add reference/references.json

# Check if there is nothing to commit (i.e. no changes to the concepts summary)
if [ -z "$(git status --porcelain)" ]; then
    echo "No changes to the concepts summary"
    exit 0
fi

# Checkout to new branch
BRANCH="bot/summaries/concepts/$(date +%Y%m%d%H%M%S)"
git checkout -b "$BRANCH"

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

# Commit and push the changes
git commit -m "[Bot] Update concepts summary"
git push origin "$BRANCH"

# Create a PR
gh pr create --title "[Bot] Update concepts summary" --body "This is an _automatically generated_ PR to update the concepts summary files." --label "type/bot"

#!/usr/bin/env sh

# Update the stories summary
dotnet run -p .github/bin/stories

# Format the documents
npx prettier@2.0.4 --write reference/stories/README.md reference/stories/stories.json

# Add the updated story summary files
git add reference/stories/README.md
git add reference/stories/stories.json

# Check if there is nothing to commit (i.e. no changes to the stories summary)
if [ -z "$(git status --porcelain)" ]; then
    echo "No changes to the stories summary"
    exit 0
fi

# Checkout to new branch
BRANCH="storysummary-$(date +%Y%m%d%H%M%S)"
git checkout -b "$BRANCH"

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

# Commit and push the changes
git commit -m "[CI] Update stories summary"
git push origin "$BRANCH"

# Create a PR
gh pr create --title "[CI] Update story summary" --body "This is an _automatically generated_ PR to update the story summary files."

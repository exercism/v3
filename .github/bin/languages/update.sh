# !/usr/bin/env sh

# Update the languages summary
dotnet run -p .github/bin/languages

# Format the documents
npx prettier@2.0.4 --write languages/README.md languages/languages.json

# Fetch and prune remote
git fetch --prune origin

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

# Checkout to new branch
BRANCH="docs/languagesummary-$(date +%Y%m%d%H%M%S)"
git checkout -b "$BRANCH"

# Add the updated language summary files
git add languages/README.md
git add languages/languages.json

# Check if there is nothing to commit (i.e. no changes to the languages summary)
if [ -z "$(git status --porcelain)" ]; then
    echo "No changes to the languages summary"
    exit 0
fi

echo "Committing updated languages summary"

# Commit and push the changes
git commit -m "[CI] Update languages summary"
git push origin "HEAD:$BRANCH"

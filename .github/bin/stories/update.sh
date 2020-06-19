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

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

echo "Committing updated stories summary"

# Commit the changes made by dotnet format
git add .
git commit -m "[CI] Update stories summary"
git push

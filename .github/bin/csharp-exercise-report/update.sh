#!/usr/bin/env sh

# Update the csharp exercises document
dotnet run -p .github/bin/csharp-exercise-report

if [ $? -eq 1 ]
then
   echo "exercise report (dot net) failed"
   exit 1
fi
# Format the documents
npx prettier@2.0.4 --write languages/csharp/reference/README.md languages/csharp/reference/exercise-errors.json

# Add the updated files
git add languages/csharp/reference/README.md
git add languages/csharp/reference/exercise-errors.json

# Check if there is nothing to commit (i.e. no changes to the files)
if [ -z "$(git status --porcelain)" ]; then
    echo "No changes to the csharp/reference/README.md or exercise-errors.json document"
    exit 0
fi

# Checkout to new branch
BRANCH="bot/update/csharp/exercises/$(date +%Y%m%d%H%M%S)"
git checkout -b "$BRANCH"

# Setup the git user (required to commit anything)
git config --global user.email "github-actions[bot]@users.noreply.github.com"
git config --global user.name "github-actions[bot]"

# Commit and push the changes
git commit -m "[Bot] Update csharp/exercise-report document"
git push origin "$BRANCH"

# Create a PR
gh pr create --title "[Bot] Update csharp/exercises document" --body "This is an _automatically generated_ PR to update the csharp exercises files." --label "type/bot" --base "master"

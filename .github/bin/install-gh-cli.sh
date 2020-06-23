# !/usr/bin/env sh

VERSION="0.10.0"
URL="https://github.com/cli/cli/releases/download/v${VERSION}/gh_${VERSION}_linux_amd64.deb"
HEADER="authorization: Bearer ${GITHUB_TOKEN}" 
OUTPUT_FILE="gh.deb"

curl --header "${HEADER}" --location "${URL}" -o "${OUTPUT_FILE}"
sudo apt install "./${OUTPUT_FILE}"
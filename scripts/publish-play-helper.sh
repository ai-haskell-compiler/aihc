#!/usr/bin/env bash
# Publish the compressed helper files of a documentation preview to R2.
#
# Usage: scripts/publish-play-helper.sh HELPER_DIR
#
# HELPER_DIR is the output of scripts/build-lazy-reader-helper.sh, built with
# AIHC_PLAY_HELPER_URL. Its tools.json names the URL of each compressed file.
# The script uploads each file and then removes it from HELPER_DIR. Thus the
# preview branch keeps only the small files. The URL has the version of the
# build, so a file never changes after its upload.
set -euo pipefail
helper=${1:?Give the helper directory.}
bucket=${AIHC_PLAY_TOOLS_BUCKET:-aihc-play-assets}
base_url=${AIHC_PLAY_HELPER_URL:-https://assets.aihc.app/play/helper}
key_prefix=${AIHC_PLAY_HELPER_KEY:-play/helper}
version=$(jq --raw-output .version "$helper/tools.json")
expected="${base_url%/}/$version/"
actual=$(jq --raw-output .helper "$helper/tools.json")
if [[ "$actual" != "$expected" ]]; then
  echo "tools.json names $actual and not $expected." >&2
  echo "Build the helper with AIHC_PLAY_HELPER_URL=$base_url." >&2
  exit 1
fi
for file in "$helper/"*.gz; do
  name=$(basename "$file")
  url="$expected$name"
  status=$(curl --silent --show-error --head --output /dev/null --write-out '%{http_code}' "$url")
  if [[ "$status" == 404 ]]; then
    # The browser decompresses .gz files. Do not set Content-Encoding.
    wrangler r2 object put "$bucket/$key_prefix/$version/$name" --remote \
      --file "$file" --content-type application/gzip \
      --cache-control 'public, max-age=31536000, immutable'
  elif [[ "$status" != 200 ]]; then
    echo "Cannot check $url: HTTP $status" >&2
    exit 1
  fi
  rm "$file"
done
echo "Helper files: $expected"

#!/usr/bin/env bash
# Publish the compressed kit files of a documentation preview to R2.
#
# Usage: scripts/publish-play-helper.sh KIT_DIR
#
# KIT_DIR is the output of scripts/build-web-kit.sh, built with
# AIHC_PLAY_HELPER_URL. Its manifest.json names the URL of each compressed
# file. The script uploads each file and then removes it from KIT_DIR. Thus
# the preview branch keeps only the small files. The URL has the build of
# the kit, so a file never changes after its upload.
set -euo pipefail
helper=${1:?Give the kit directory.}
bucket=${AIHC_PLAY_TOOLS_BUCKET:-aihc-play-assets}
base_url=${AIHC_PLAY_HELPER_URL:-https://assets.aihc.app/play/helper}
key_prefix=${AIHC_PLAY_HELPER_KEY:-play/helper}
version=$(jq --raw-output .build "$helper/manifest.json")
expected="${base_url%/}/$version/"
for file in "$helper/"*.gz; do
  name=$(basename "$file")
  url="$expected$name"
  actual=$(jq --raw-output --arg path "$name" '.files[] | select(.path == $path) | .url' "$helper/manifest.json")
  if [[ "$actual" != "$url" ]]; then
    echo "manifest.json puts $name at $actual and not at $url." >&2
    echo "Build the kit with AIHC_PLAY_HELPER_URL=$base_url." >&2
    exit 1
  fi
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

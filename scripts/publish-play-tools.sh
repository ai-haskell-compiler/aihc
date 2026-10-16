#!/usr/bin/env bash
# Publish immutable browser tools before the site refers to them.
set -euo pipefail
tools=${AIHC_PLAY_TOOLS:?The Nix app must supply AIHC_PLAY_TOOLS.}
bucket=${AIHC_PLAY_TOOLS_BUCKET:-aihc-play-assets}
base_url=${AIHC_PLAY_TOOLS_URL:-https://assets.aihc.app/play}
version=$(cat "$tools/version")
for file in "$tools/assets/"*; do
  name=$(basename "$file")
  url="${base_url%/}/$version/$name"
  status=$(curl --silent --show-error --head --output /dev/null --write-out '%{http_code}' "$url")
  if [[ "$status" == 200 ]]; then
    continue
  fi
  if [[ "$status" != 404 ]]; then
    echo "Cannot check $url: HTTP $status" >&2
    exit 1
  fi
  case "$name" in
    *.js) content_type=application/javascript ;;
    *.wasm) content_type=application/wasm ;;
    *.gz) content_type=application/gzip ;;
    *) content_type=text/plain ;;
  esac
  # The browser decompresses .gz files. Do not set Content-Encoding.
  wrangler r2 object put "$bucket/play/$version/$name" --remote \
    --file "$file" --content-type "$content_type" \
    --cache-control 'public, max-age=31536000, immutable'
done
echo "Browser tools: ${base_url%/}/$version/tools.js"

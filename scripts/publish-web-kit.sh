#!/usr/bin/env bash
# Publish the web kit of a release to R2, under its version.
#
# Usage: scripts/publish-web-kit.sh KIT_DIR VERSION [--dry-run]
#
# KIT_DIR is an unpacked aihc-VERSION-web archive. The script uploads each
# file to play/releases/VERSION/ of the bucket, which assets.aihc.app serves.
# A website pins VERSION, so a published file must never change. Thus the
# script does not replace a file and does not delete a file:
#   - When manifest.json of VERSION is there and is the same, the script stops.
#   - When it is there and is different, the script fails.
#   - When a file is there with the SHA-256 of the manifest, the script
#     skips it. This occurs after a run that stopped early.
#   - When a file is there with a different SHA-256, the script fails.
# manifest.json goes last. Thus a manifest on the host means a complete kit.
#
# With --dry-run, the script does the checks and prints each upload.
set -euo pipefail
kit=${1:?Give the kit directory.}
version=${2:?Give the version.}
dry_run=${3:-}
bucket=${AIHC_PLAY_TOOLS_BUCKET:-aihc-play-assets}
base_url=${AIHC_WEB_KIT_URL:-https://assets.aihc.app/play/releases}
key_prefix=${AIHC_WEB_KIT_KEY:-play/releases}
target="${base_url%/}/$version"

if [[ $(jq --raw-output .version "$kit/manifest.json") != "$version" ]]; then
  echo "manifest.json is not for version $version." >&2
  exit 1
fi
if jq --exit-status 'any(.files[]; has("url"))' "$kit/manifest.json" >/dev/null; then
  echo "manifest.json puts a file outside the kit. Build the kit without AIHC_PLAY_TOOLS_URL and AIHC_PLAY_HELPER_URL." >&2
  exit 1
fi

status_of() {
  curl --silent --show-error --head --output /dev/null --write-out '%{http_code}' "$1"
}

put() {
  local path=$1 content_type
  case "$path" in
    *.js) content_type=text/javascript ;;
    *.json) content_type=application/json ;;
    *.wasm) content_type=application/wasm ;;
    *.gz) content_type=application/gzip ;;
    *.md) content_type=text/markdown ;;
    *) content_type=text/plain ;;
  esac
  if [[ "$dry_run" == --dry-run ]]; then
    echo "Dry run: upload $path as $content_type"
    return
  fi
  # The browser decompresses .gz files. Do not set Content-Encoding.
  wrangler r2 object put "$bucket/$key_prefix/$version/$path" --remote \
    --file "$kit/$path" --content-type "$content_type" \
    --cache-control 'public, max-age=31536000, immutable'
}

status=$(status_of "$target/manifest.json")
if [[ "$status" == 200 ]]; then
  if curl --silent --show-error --fail "$target/manifest.json" | cmp --silent - "$kit/manifest.json"; then
    echo "Version $version is already at $target/ with the same manifest."
    exit 0
  fi
  echo "Version $version is already at $target/ with a different manifest." >&2
  echo "A published version must not change. Release a new version." >&2
  exit 1
elif [[ "$status" != 404 ]]; then
  echo "Cannot check $target/manifest.json: HTTP $status" >&2
  exit 1
fi

while IFS=$'\t' read -r path sha256; do
  status=$(status_of "$target/$path")
  if [[ "$status" == 200 ]]; then
    actual=$(curl --silent --show-error --fail "$target/$path" | sha256sum | cut -d' ' -f1)
    if [[ "$actual" != "$sha256" ]]; then
      echo "$target/$path is there with a different SHA-256." >&2
      exit 1
    fi
    echo "Skip $path, which is already there."
  elif [[ "$status" == 404 ]]; then
    put "$path"
  else
    echo "Cannot check $target/$path: HTTP $status" >&2
    exit 1
  fi
done < <(jq --raw-output '.files[] | [.path, .sha256] | @tsv' "$kit/manifest.json")
put manifest.json
echo "Web kit: $target/manifest.json"

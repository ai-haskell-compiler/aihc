// The manifest of the web kit, and the downloads that it lists.
//
// manifest.json lists each file of the kit with its size and SHA-256. The
// browser keeps the large files in Cache Storage. The name of the cache has
// the build of the kit, so a new build gets a new cache.

const PREFIX = "aihc-web-kit-";
const networkFetch = globalThis.fetch.bind(globalThis);

export const SCHEMA = 1;

// Fetch and decode manifest.json. Each file gets an absolute URL in href.
export async function loadManifest(manifestUrl) {
  const url = new URL(manifestUrl, globalThis.location?.href);
  const response = await networkFetch(url, { cache: "no-cache" });
  if (!response.ok) throw new Error("Cannot load " + url.href + ": HTTP " + response.status);
  const config = await response.json();
  if (config.schema !== SCHEMA) throw new Error("This kit glue reads manifest schema " + SCHEMA + ", not " + config.schema + ".");
  const files = new Map();
  for (const file of config.files) files.set(file.path, { ...file, href: new URL(file.url ?? file.path, url).href });
  return { ...config, url: url.href, files };
}

// Cache Storage is not available in all contexts, for example in some
// private windows. Then the files come from the network each time.
async function openCache(build) {
  try {
    return build && globalThis.caches ? await caches.open(PREFIX + build) : null;
  } catch {
    return null;
  }
}

const hex = (buffer) => [...new Uint8Array(buffer)].map((byte) => byte.toString(16).padStart(2, "0")).join("");

// The downloads of one manifest. The page imports a module file, and the
// browser keeps it in the HTTP cache. Each other file goes through fetch.
export function createDownloads(manifest) {
  const byHref = new Map([...manifest.files.values()].map((file) => [file.href, file]));
  const file = (path) => {
    const entry = manifest.files.get(path);
    if (!entry) throw new Error("The manifest does not list " + path + ".");
    return entry;
  };

  // Fetch a URL. A file of the manifest comes from the cache when possible.
  // A new download must have the size and the SHA-256 of the manifest.
  async function fetchCached(input, init) {
    const href = new URL(input instanceof Request ? input.url : input, globalThis.location?.href).href;
    const entry = byHref.get(href);
    if (!entry || entry.module) return networkFetch(input, init);
    const cache = await openCache(manifest.build);
    const stored = await cache?.match(href).catch(() => undefined);
    if (stored) return stored;
    const response = await networkFetch(input, init);
    if (!response.ok) return response;
    const bytes = await response.arrayBuffer();
    if (globalThis.crypto?.subtle) {
      const digest = hex(await crypto.subtle.digest("SHA-256", bytes));
      if (bytes.byteLength !== entry.bytes || digest !== entry.sha256) throw new Error("The download of " + entry.path + " does not agree with the manifest.");
    }
    const headers = { "content-type": response.headers.get("content-type") ?? "application/octet-stream" };
    // Do not wait for the cache. The caller reads the body at the same time.
    cache?.put(href, new Response(bytes, { headers })).catch(() => {});
    return new Response(bytes, { headers });
  }

  async function bytes(path) {
    const entry = file(path);
    const response = await fetchCached(entry.href);
    if (!response.ok) throw new Error("Cannot load " + path + ": HTTP " + response.status);
    return response;
  }

  // The contents of a .gz file. The kit serves these files without
  // Content-Encoding, so the size on the wire does not depend on the server.
  async function inflated(path) {
    const response = await bytes(path);
    return new Response(response.body.pipeThrough(new DecompressionStream("gzip"))).arrayBuffer();
  }

  // The number of bytes that the groups need, and the number that the
  // browser does not have. A page cannot examine the HTTP cache. Thus a
  // module counts as missing only when a cached file is missing. This also
  // removes the caches of earlier builds.
  async function downloadSize(groups = ["glue", "compiler", "linker"]) {
    const cache = await openCache(manifest.build);
    try {
      for (const name of await caches.keys()) {
        if (name.startsWith(PREFIX) && name !== PREFIX + manifest.build) await caches.delete(name);
      }
    } catch {}
    let total = 0, missing = 0, modules = 0;
    for (const entry of manifest.files.values()) {
      if (!groups.includes(entry.group)) continue;
      total += entry.bytes;
      if (entry.module) modules += entry.bytes;
      else if (!cache || !(await cache.match(entry.href).catch(() => undefined))) missing += entry.bytes;
    }
    return { total, missing: missing > 0 ? missing + modules : 0 };
  }

  return { file, fetch: fetchCached, inflated, downloadSize };
}

// Decode a pack: the container of the linker inputs. The format is in the
// README of the kit. The result maps each name to a view of the bytes.
export function decodePack(buffer) {
  const all = new Uint8Array(buffer);
  if (new TextDecoder().decode(all.subarray(0, 8)) !== "AIHCPACK") throw new Error("The file is not a pack.");
  const view = new DataView(all.buffer, all.byteOffset, all.byteLength);
  if (view.getUint32(8, true) !== 1) throw new Error("The pack has an unknown version.");
  const length = view.getUint32(12, true);
  const index = JSON.parse(new TextDecoder().decode(all.subarray(16, 16 + length)));
  const base = 16 + length;
  return Object.fromEntries(index.files.map(({ name, offset, bytes }) => [name, all.subarray(base + offset, base + offset + bytes)]));
}

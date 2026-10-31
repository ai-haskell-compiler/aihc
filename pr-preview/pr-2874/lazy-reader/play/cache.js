// The browser keeps the large files of the playgrounds in Cache Storage.
// Thus a later visit can find out whether the files are already in the
// browser. helper/tools.json lists the files. Its version is in the name of
// the cache, so a new build gets a new cache.

const PREFIX = "aihc-play-";
const MANIFEST = new URL("helper/tools.json", import.meta.url);
const networkFetch = globalThis.fetch.bind(globalThis);
let manifestPromise;

export function manifest() {
  manifestPromise ??= (async () => {
    const response = await networkFetch(MANIFEST, { cache: "no-cache" });
    if (!response.ok) throw new Error("Cannot load the tool configuration: " + response.status);
    const config = await response.json();
    const files = new Map((config.files ?? []).map((file) => [new URL(file.url, MANIFEST).href, file]));
    return { url: new URL(config.url, MANIFEST).href, version: config.version, files };
  })();
  manifestPromise.catch(() => { manifestPromise = null; });
  return manifestPromise;
}

// Cache Storage is not available in all contexts, for example in some
// private windows. Then the files come from the network each time.
async function openCache(version) {
  try {
    return version && globalThis.caches ? await caches.open(PREFIX + version) : null;
  } catch {
    return null;
  }
}

// A fetch that keeps each listed file in the cache. Other URLs go to the
// network.
export async function cachedFetch(input, init) {
  const url = new URL(input instanceof Request ? input.url : input, globalThis.location?.href).href;
  let config;
  try {
    config = await manifest();
  } catch {
    return networkFetch(input, init);
  }
  const file = config.files.get(url);
  const cache = file && !file.module ? await openCache(config.version) : null;
  if (!cache) return networkFetch(input, init);
  const stored = await cache.match(url).catch(() => undefined);
  if (stored) return stored;
  const response = await networkFetch(input, init);
  // Do not wait for the cache. The caller reads the body at the same time.
  if (response.ok) cache.put(url, response.clone()).catch(() => {});
  return response;
}

// The number of bytes that the playground must download, and the total.
// The browser keeps imported modules in its HTTP cache, which a page cannot
// examine. Thus they count as missing only when a cached file is missing.
// This also removes the caches of earlier versions.
export async function downloadSize() {
  const config = await manifest();
  const cache = await openCache(config.version);
  try {
    for (const name of await caches.keys()) {
      if (name.startsWith(PREFIX) && name !== PREFIX + config.version) await caches.delete(name);
    }
  } catch {}
  let total = 0, missing = 0, modules = 0;
  for (const [url, file] of config.files) {
    total += file.bytes;
    if (file.module) modules += file.bytes;
    else if (!cache || !(await cache.match(url).catch(() => undefined))) missing += file.bytes;
  }
  return { total, missing: missing > 0 ? missing + modules : 0 };
}

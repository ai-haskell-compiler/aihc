import { instantiate } from "./helper/aihc-play-component.js";
import { imports } from "./host.js";
import { cachedFetch } from "./cache.js";

const instance = await instantiate(async (name) => {
  const response = await cachedFetch(new URL("helper/" + name + ".gz", import.meta.url));
  if (!response.ok) throw new Error("Cannot load the compiler: " + response.status);
  const bytes = await new Response(response.body.pipeThrough(new DecompressionStream("gzip"))).arrayBuffer();
  return WebAssembly.compile(bytes);
}, imports);

export const run = instance.run;

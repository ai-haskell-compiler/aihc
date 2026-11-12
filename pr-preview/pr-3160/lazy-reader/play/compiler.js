import { instantiate } from "./helper/aihc-play-component.js";
import { imports } from "./host.js";
import { cachedFetch, helperUrl } from "./cache.js";

const instance = await instantiate(async (name) => {
  const response = await cachedFetch(await helperUrl(name + ".gz"));
  if (!response.ok) throw new Error("Cannot load the compiler: " + response.status);
  const bytes = await new Response(response.body.pipeThrough(new DecompressionStream("gzip"))).arrayBuffer();
  return WebAssembly.compile(bytes);
}, imports);

export const run = instance.run;

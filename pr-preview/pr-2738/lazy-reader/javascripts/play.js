// The playgrounds of The Lazy Reader.
//
// Each element with the class "lazy-play" is one playground. Its
// data-command attribute names the helper command: parse, names, or
// types. The seed source is the text of its <pre> child. The script builds
// an editor and an output pane, and asks the helper again after each edit.
//
// The helper is aihc itself, compiled to WebAssembly. It loads once per
// page, when the first playground scrolls into view.

const PLAY_ROOT = new URL("../play/", import.meta.url);
const programWorkers = new Set();

// ---------------------------------------------------------------------------
// The helper

const helper = {
  state: "idle",
  reason: "",
  queue: Promise.resolve(),
  pending: null,
  inputs: [],
  waiters: [],
  lineBuffer: "",
  decoder: new TextDecoder(),
  encoder: new TextEncoder(),
  listeners: new Set(),
};

function setHelperState(state, reason = "") {
  helper.state = state;
  helper.reason = reason;
  for (const listener of helper.listeners) listener(state, reason);
}

// The helper reads its input through a buffer of 8192 bytes, in pieces
// of at most this many bytes. A larger piece than the read of the helper
// loses its tail in the stream of the host.
const INPUT_CHUNK = 4096;

// A request body of at least the buffer size is read by the helper in
// one read of the whole body, so the host hands it over in one piece.
const DIRECT_READ = 8192;

function pushPiece(piece) {
  if (helper.waiters.length > 0) {
    helper.waiters.shift()(piece);
  } else {
    helper.inputs.push(piece);
  }
}

function pushInput(bytes) {
  for (let offset = 0; offset < bytes.length; offset += INPUT_CHUNK) {
    pushPiece(bytes.subarray(offset, offset + INPUT_CHUNK));
  }
}

function nextInput() {
  if (helper.inputs.length > 0) return Promise.resolve(helper.inputs.shift());
  return new Promise((resolve) => helper.waiters.push(resolve));
}

function onStdout(bytes) {
  helper.lineBuffer += helper.decoder.decode(bytes, { stream: true });
  let newline;
  while ((newline = helper.lineBuffer.indexOf("\n")) >= 0) {
    const line = helper.lineBuffer.slice(0, newline);
    helper.lineBuffer = helper.lineBuffer.slice(newline + 1);
    const pending = helper.pending;
    helper.pending = null;
    if (pending) {
      try {
        pending.resolve(JSON.parse(line));
      } catch (error) {
        pending.reject(new Error("the helper gave an unreadable answer: " + line.slice(0, 200)));
      }
    }
  }
}

function onStderr(bytes) {
  console.warn("aihc-play:", helper.decoder.decode(bytes));
}

// Hand a request to the helper: the header line, then the body. A large
// body goes in one piece after the header, so the helper reads it in one
// read; a small one goes with the header in pieces the buffer holds.
function sendFrame(command, body) {
  const header = helper.encoder.encode(command + " " + body.length + "\n");
  if (body.length >= DIRECT_READ) {
    pushPiece(header);
    pushPiece(body);
    return;
  }
  const bytes = new Uint8Array(header.length + body.length);
  bytes.set(header, 0);
  bytes.set(body, header.length);
  pushInput(bytes);
}

// One request at a time. The helper answers in order, so a queue keeps
// each answer with its request.
function send(command, body) {
  const run = () =>
    new Promise((resolve, reject) => {
      const started = performance.now();
      helper.pending = {
        resolve: (answer) => {
          console.debug("The Lazy Reader: " + command + " answered in " + Math.round(performance.now() - started) + " ms");
          resolve(answer);
        },
        reject,
      };
      sendFrame(command, body);
    });
  const result = helper.queue.then(run, run);
  helper.queue = result.catch(() => {});
  return result;
}

async function fetchText(path) {
  const response = await fetch(new URL(path, PLAY_ROOT));
  if (!response.ok) throw new Error("cannot fetch " + path + ": " + response.status);
  return response.text();
}

// The bundle of the core libraries: the naming and type facts of
// aihc-prim and aihc-base, written by the helper build. The file is gzip,
// inflated here, so the size on the wire does not depend on the server.
async function loadBundle() {
  const response = await fetch(new URL("helper/core.bundle.gz", PLAY_ROOT));
  if (!response.ok) throw new Error("cannot fetch the bundle of the core libraries: " + response.status);
  const inflated = response.body.pipeThrough(new DecompressionStream("gzip"));
  return new Uint8Array(await new Response(inflated).arrayBuffer());
}

let startPromise = null;

function startHelper() {
  if (startPromise) return startPromise;
  startPromise = (async () => {
    if (typeof WebAssembly === "undefined" || typeof WebAssembly.Suspending !== "function") {
      setHelperState(
        "unsupported",
        "This browser needs JavaScript Promise Integration to run the playground.",
      );
      return;
    }
    setHelperState("loading", "Loading the compiler");
    const started = performance.now();
    try {
      globalThis.lazyReaderHost = { nextInput, write: onStdout, writeErr: onStderr };
      const [module, bundle] = await Promise.all([import(new URL("compiler.js", PLAY_ROOT)), loadBundle()]);
      const fetched = performance.now();
      // The helper runs until the page goes away. Its promise settles only
      // on a failure.
      module.run
        .run()
        .then(() => setHelperState("stopped", "The compiler stopped."))
        .catch((error) => setHelperState("failed", "The compiler failed: " + error));
      setHelperState("loading", "Loading the core libraries.");
      const answer = await send("load-bundle", bundle);
      if (!answer.ok) throw new Error(answer.message);
      const ready = performance.now();
      helper.timing = { fetched: Math.round(fetched - started), ready: Math.round(ready - fetched) };
      console.info(
        "The Lazy Reader: the compiler is ready " +
          Math.round(ready - started) +
          " ms after the start (" +
          helper.timing.fetched +
          " ms to fetch, " +
          helper.timing.ready +
          " ms to load, " +
          answer.facts +
          " facts in " +
          answer.typeBytes +
          " bytes)",
      );
      setHelperState("ready");
    } catch (error) {
      setHelperState("failed", "The compiler did not start: " + error);
    }
  })();
  return startPromise;
}

async function ask(command, source) {
  await startHelper();
  if (helper.state !== "ready") throw new Error(helper.reason);
  return send(command, helper.encoder.encode(source));
}

// ---------------------------------------------------------------------------
// Rendering helpers

function el(tag, attributes = {}, children = []) {
  const node = document.createElement(tag);
  for (const [key, value] of Object.entries(attributes)) {
    if (key === "class") node.className = value;
    else if (key === "text") node.textContent = value;
    else node.setAttribute(key, value);
  }
  for (const child of children) node.append(child);
  return node;
}

function lineColumnOffsets(source) {
  const offsets = [0];
  for (let i = 0; i < source.length; i++) {
    if (source[i] === "\n") offsets.push(i + 1);
  }
  return offsets;
}

// Paint spans of the source. Each mark is {line, col, endLine, endCol,
// attributes}. Lines and columns are 1-based; the end column is exclusive.
function paintSource(source, marks) {
  const offsets = lineColumnOffsets(source);
  const at = (line, col) => {
    const base = offsets[Math.min(line, offsets.length) - 1] ?? source.length;
    return Math.min(source.length, base + col - 1);
  };
  const ranges = marks
    .map((mark) => ({ start: at(mark.line, mark.col), end: at(mark.endLine, mark.endCol), mark }))
    .filter((range) => range.end > range.start)
    .sort((a, b) => a.start - b.start);
  const pre = el("pre", { class: "lazy-painted" });
  let cursor = 0;
  for (const range of ranges) {
    if (range.start < cursor) continue;
    if (range.start > cursor) pre.append(document.createTextNode(source.slice(cursor, range.start)));
    const span = el("span", range.mark.attributes, [source.slice(range.start, range.end)]);
    pre.append(span);
    cursor = range.end;
  }
  if (cursor < source.length) pre.append(document.createTextNode(source.slice(cursor)));
  return pre;
}

function problemsList(problems) {
  if (!problems || problems.length === 0) return null;
  return el(
    "ul",
    { class: "lazy-problems" },
    problems.map((problem) =>
      el("li", {}, [
        el("span", { class: "lazy-problem-stage", text: problem.stage }),
        " ",
        problem.line ? el("span", { class: "lazy-problem-where", text: "line " + problem.line + ": " }) : "",
        problem.message,
      ]),
    ),
  );
}

// ---------------------------------------------------------------------------
// The parse tree

const FRIENDLY_LABELS = {
  Module: "module",
  FunctionBind: "function",
  PatternBind: "binding",
  Match: "equation",
  UnguardedRhs: "right-hand side",
  GuardedRhs: "guarded right-hand side",
  DeclTypeSig: "type signature",
  DeclData: "data type",
  DeclNewtype: "newtype",
  DeclClass: "class",
  DeclInstance: "instance",
  DeclTypeSyn: "type synonym",
  DeclFixity: "fixity",
  EVar: "variable",
  ECon: "constructor",
  EApp: "application",
  EInfixApp: "infix application",
  ELam: "lambda",
  ELet: "let",
  EIf: "if",
  ECase: "case",
  EDo: "do",
  ETuple: "tuple",
  EList: "list",
  ENeg: "negation",
  EInt: "number",
  EFloat: "number",
  EChar: "character",
  EString: "string",
  ETypeSig: "type annotation",
  ELeftSection: "left section",
  ERightSection: "right section",
  PVar: "variable",
  PCon: "constructor pattern",
  PWildcard: "wildcard",
  PLit: "literal pattern",
  PTuple: "tuple pattern",
  PList: "list pattern",
  PInfix: "infix pattern",
  PAs: "as-pattern",
  PBang: "bang pattern",
  TyVar: "type variable",
  TyCon: "type constructor",
  TyApp: "type application",
  TyFun: "function type",
  TyList: "list type",
  TyTuple: "tuple type",
  TyForall: "forall",
  Name: "name",
  ModuleHead: "module header",
  ImportDecl: "import",
};

function friendlyLabel(label) {
  if (FRIENDLY_LABELS[label]) return FRIENDLY_LABELS[label];
  // "DeclStandaloneDeriving" becomes "standalone deriving".
  const words = label.replace(/^(Decl|E|P|Ty|Lit|T)(?=[A-Z])/, "").replace(/([a-z])([A-Z])/g, "$1 $2");
  return words.toLowerCase();
}

function renderTree(node, isLast = true, isRoot = true) {
  const row = el("div", { class: "lazy-tree-node" });
  row.append(el("span", { class: "lazy-tree-label", text: friendlyLabel(node.label) }));
  if (node.text !== undefined) row.append(el("code", { class: "lazy-tree-text", text: node.text }));
  const classes = ["lazy-tree-item"];
  if (isRoot) classes.push("lazy-tree-root");
  if (isLast) classes.push("lazy-tree-last");
  const item = el("div", { class: classes.join(" ") }, [row]);
  const kids = node.children ?? [];
  if (kids.length > 0) {
    const children = el("div", { class: "lazy-tree-children" });
    kids.forEach((child, index) => children.append(renderTree(child, index === kids.length - 1, false)));
    item.append(children);
  }
  return item;
}

// ---------------------------------------------------------------------------
// Names and colors

const PALETTE = [
  "#d94f3d", "#2a7fb8", "#2f9e6b", "#c7791a", "#8e5bc2", "#1f9c9c",
  "#c94a8c", "#6d8a17", "#b35c2e", "#4461d6", "#9b2e6d", "#3b8f3b",
];

function targetKey(target) {
  switch (target.kind) {
    case "local":
      return "local:" + target.id;
    case "global":
      return "global:" + target.module + "." + target.name;
    case "syntax":
      return "syntax";
    default:
      return "unresolved";
  }
}

function describeTarget(occurrence, binders) {
  const target = occurrence.target;
  switch (target.kind) {
    case "local": {
      const binder = binders.get(targetKey(target));
      return binder ? "bound on line " + binder.line : "a local name";
    }
    case "global":
      if (target.package === "" || target.package === "main") {
        const binder = binders.get(targetKey(target));
        return binder ? "defined on line " + binder.line : "defined in this module";
      }
      return "from " + target.module;
    case "syntax":
      return "built-in syntax";
    default:
      return "not found: " + target.message;
  }
}

function renderNames(source, answer) {
  const binders = new Map();
  for (const occurrence of answer.names) {
    if (occurrence.binder) {
      const key = targetKey(occurrence.target);
      if (!binders.has(key)) binders.set(key, occurrence);
    }
  }
  const colors = new Map();
  const legend = [];
  const marks = [];
  for (const occurrence of answer.names) {
    const key = targetKey(occurrence.target);
    const unresolved = occurrence.target.kind === "unresolved";
    if (!colors.has(key)) {
      colors.set(key, unresolved ? "var(--lazy-bad)" : PALETTE[colors.size % PALETTE.length]);
      legend.push({ key, occurrence, color: colors.get(key) });
    }
    const classes = ["lazy-name"];
    if (occurrence.binder) classes.push("lazy-name-binder");
    if (unresolved) classes.push("lazy-name-unresolved");
    marks.push({
      line: occurrence.line,
      col: occurrence.col,
      endLine: occurrence.endLine,
      endCol: occurrence.endCol,
      attributes: {
        class: classes.join(" "),
        style: "--name-color: " + colors.get(key),
        "data-key": key,
        title: occurrence.text + ": " + describeTarget(occurrence, binders),
      },
    });
  }
  const painted = paintSource(source, marks);
  const list = el("ul", { class: "lazy-legend" });
  for (const entry of legend) {
    const item = el("li", { "data-key": entry.key, style: "--name-color: " + entry.color }, [
      el("span", { class: "lazy-legend-swatch" }),
      el("code", { text: entry.occurrence.text }),
      el("span", { class: "lazy-legend-note", text: describeTarget(entry.occurrence, binders) }),
    ]);
    list.append(item);
  }
  const hover = (key, on) => {
    for (const node of painted.querySelectorAll("[data-key]")) {
      node.classList.toggle("lazy-name-dim", on && node.dataset.key !== key);
    }
    for (const node of list.querySelectorAll("[data-key]")) {
      node.classList.toggle("lazy-legend-active", on && node.dataset.key === key);
    }
  };
  for (const node of [...painted.querySelectorAll("[data-key]"), ...list.querySelectorAll("[data-key]")]) {
    node.addEventListener("mouseenter", () => hover(node.dataset.key, true));
    node.addEventListener("mouseleave", () => hover(node.dataset.key, false));
  }
  return el("div", { class: "lazy-names" }, [painted, list]);
}

// ---------------------------------------------------------------------------
// Types

function renderTypes(answer) {
  const lines = answer.source.replace(/\n$/, "").split("\n");
  const inserted = new Set();
  // Each signature sits above its binding. The binding lines move down by
  // the number of signatures above them.
  let shift = 0;
  const bindingLines = answer.signatures.map((signature) => signature.line);
  for (let i = 0; i < bindingLines.length; i++) {
    inserted.add(bindingLines[i] + shift);
    shift += 1;
  }
  const pre = el("pre", { class: "lazy-painted lazy-typed" });
  lines.forEach((line, index) => {
    const number = index + 1;
    pre.append(el("span", { class: "lazy-line" + (inserted.has(number) ? " lazy-inferred" : "") }, [line]));
  });
  return pre;
}

// ---------------------------------------------------------------------------
// The playground element

function buildPlayground(root) {
  const command = root.dataset.command;
  const seed = root.querySelector("pre")?.textContent.replace(/\n$/, "") ?? "";
  root.textContent = "";
  const editor = el("textarea", {
    class: "lazy-editor",
    spellcheck: "false",
    wrap: "off",
    autocapitalize: "off",
    autocomplete: "off",
    rows: String(Math.max(3, seed.split("\n").length + 1)),
    "aria-label": "Haskell source",
  });
  editor.value = seed;
  const status = el("div", { class: "lazy-status", text: "" });
  const output = el("div", { class: "lazy-output" });
  root.append(
    el("div", { class: "lazy-pane lazy-pane-input" }, [el("div", { class: "lazy-pane-title", text: "You write" }), editor]),
    el("div", { class: "lazy-pane lazy-pane-output" }, [
      el("div", { class: "lazy-pane-title", text: PANE_TITLES[command] ?? "The compiler answers" }),
      status,
      output,
    ]),
  );

  let timer = null;
  let generation = 0;

  if (command === "compile") {
    const run = el("button", { type: "button", class: "lazy-run", text: "Run" });
    const stop = el("button", { type: "button", class: "lazy-run", text: "Stop" });
    stop.disabled = true;
    root.querySelector(".lazy-pane-input").append(el("div", { class: "lazy-run-controls" }, [run, stop]));
    status.setAttribute("role", "status");
    output.setAttribute("aria-label", "Program output");
    status.textContent = "Select Run to compile the module.";
    let worker;
    const finish = () => {
      worker?.terminate();
      programWorkers.delete(worker);
      worker = null;
      run.disabled = false;
      stop.disabled = true;
    };
    run.addEventListener("click", () => {
      finish();
      output.textContent = "";
      const stdout = el("pre", { class: "lazy-program-stdout" });
      const stderr = el("pre", { class: "lazy-program-stderr" });
      output.append(stdout, stderr);
      let count = 0;
      run.disabled = true;
      stop.disabled = false;
      status.textContent = "Load the compiler.";
      worker = new Worker(new URL("helper/run.js", PLAY_ROOT), { type: "module" });
      const current = worker;
      programWorkers.add(worker);
      worker.onmessage = ({ data }) => {
        if (worker !== current) return;
        if (data.type === "status") status.textContent = data.text;
        if (data.type === "stdout" || data.type === "stderr" || data.type === "tool-stderr" || data.type === "compiler-stderr") {
          const pane = data.type === "stdout" ? stdout : stderr;
          const text = data.text.slice(0, Math.max(0, 65536 - count));
          pane.append(document.createTextNode(text));
          count += data.text.length;
          if (count > 65536) {
            status.textContent = "The output reached the size limit. The program stopped.";
            finish();
          }
        }
        if (data.type === "diagnostics") {
          output.append(problemsList(data.errors));
          status.textContent = "Correct the module, then select Run.";
          finish();
        }
        if (data.type === "done" || data.type === "error") {
          status.textContent = data.text;
          finish();
        }
      };
      worker.onerror = (error) => {
        if (worker !== current) return;
        status.textContent = error.message;
        finish();
      };
      worker.postMessage(editor.value);
    });
    stop.addEventListener("click", () => {
      finish();
      status.textContent = "The program stopped.";
    });
  }

  const show = (answer, source) => {
    output.textContent = "";
    const problems = problemsList(answer.errors);
    if (command === "parse" && answer.tree && answer.errors.length === 0) output.append(renderTree(answer.tree));
    if (command === "names" && answer.names) output.append(renderNames(source, answer));
    if (command === "types" && answer.source && answer.errors.length === 0) output.append(renderTypes(answer));
    if (problems) output.append(problems);
  };

  const refresh = async () => {
    const source = editor.value;
    const mine = ++generation;
    try {
      const answer = await ask(command, source);
      if (mine !== generation) return;
      status.textContent = "";
      if (!answer.ok) {
        status.textContent = answer.message;
        return;
      }
      show(answer, source);
    } catch (error) {
      if (mine !== generation) return;
      status.textContent = String(error.message ?? error);
    }
  };

  editor.addEventListener("input", () => {
    editor.rows = Math.max(3, editor.value.split("\n").length + 1);
    clearTimeout(timer);
    if (command !== "compile") timer = setTimeout(refresh, 120);
  });
  editor.addEventListener("keydown", (event) => {
    // Keep the page shortcuts of the theme away from the editor.
    event.stopPropagation();
    if ((event.key === "Home" || event.key === "End") && !event.altKey && !event.metaKey) {
      // On macOS these keys move to the start or end of the whole text.
      // Move along the line instead, and along the text with Control.
      event.preventDefault();
      const { value, selectionStart, selectionEnd, selectionDirection } = editor;
      const head = selectionDirection === "backward" ? selectionStart : selectionEnd;
      let target;
      if (event.key === "Home") {
        target = event.ctrlKey ? 0 : value.lastIndexOf("\n", head - 1) + 1;
      } else {
        const newline = value.indexOf("\n", head);
        target = event.ctrlKey || newline < 0 ? value.length : newline;
      }
      if (event.shiftKey) {
        const anchor = selectionDirection === "backward" ? selectionEnd : selectionStart;
        editor.setSelectionRange(Math.min(anchor, target), Math.max(anchor, target), target < anchor ? "backward" : "forward");
      } else {
        editor.setSelectionRange(target, target);
      }
      return;
    }
    if (event.key === "Enter" && !event.shiftKey && !event.metaKey && !event.ctrlKey && !event.altKey) {
      // Keep the indentation of the current line.
      event.preventDefault();
      const { selectionStart, selectionEnd, value } = editor;
      const lineStart = value.lastIndexOf("\n", selectionStart - 1) + 1;
      const indent = /^[ \t]*/.exec(value.slice(lineStart, selectionStart))[0];
      editor.setRangeText("\n" + indent, selectionStart, selectionEnd, "end");
      editor.dispatchEvent(new Event("input"));
      return;
    }
    if (event.key === "Tab") {
      event.preventDefault();
      editor.setRangeText("  ", editor.selectionStart, editor.selectionEnd, "end");
      editor.dispatchEvent(new Event("input"));
    }
  });

  if (command !== "compile") helper.listeners.add((state, reason) => {
    if (state === "ready") {
      status.textContent = "";
      refresh();
    } else {
      status.textContent = reason;
    }
  });

  return { refresh: command === "compile" ? () => {} : refresh };
}

const PANE_TITLES = {
  parse: "The compiler sees",
  names: "The compiler knows",
  types: "The compiler infers",
  compile: "Program output",
};

// The animation of chapter 1 plays one round, waits, and plays again. It
// starts when it scrolls into view.
function initAnimations() {
  for (const figure of document.querySelectorAll(".lazy-anim")) {
    if (figure.dataset.armed || matchMedia("(prefers-reduced-motion: reduce)").matches) continue;
    figure.dataset.armed = "yes";
    figure.classList.add("armed");
    let timer = null;
    const round = () => {
      figure.classList.remove("on");
      // Force a style flush, so the reset is not merged with the next round.
      void figure.getBoundingClientRect();
      timer = setTimeout(() => {
        figure.classList.add("on");
        timer = setTimeout(round, 11000);
      }, 400);
    };
    new IntersectionObserver((entries) => {
      const visible = entries.some((entry) => entry.isIntersecting);
      clearTimeout(timer);
      if (visible) round();
    }).observe(figure);
  }
}

function init() {
  for (const worker of programWorkers) worker.terminate();
  programWorkers.clear();
  initAnimations();
  const roots = [...document.querySelectorAll(".lazy-play")];
  if (roots.length === 0) return;
  const playgrounds = roots.map(buildPlayground);
  const observer = new IntersectionObserver((entries) => {
    if (entries.some((entry) => entry.isIntersecting)) {
      observer.disconnect();
      startHelper().then(() => {
        if (helper.state === "ready") playgrounds.forEach((playground) => playground.refresh());
      });
    }
  });
  roots.filter((root) => root.dataset.command !== "compile").forEach((root) => observer.observe(root));
}

// Material loads pages with instant navigation, so the script runs once and
// listens for each page.
if (typeof document$ !== "undefined") {
  document$.subscribe(init);
} else {
  init();
}

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

// The helper reads its input in pieces of at most this many bytes. A
// larger piece than the read buffer of the helper loses its tail in the
// stream of the host.
const INPUT_CHUNK = 4096;

function pushInput(bytes) {
  for (let offset = 0; offset < bytes.length; offset += INPUT_CHUNK) {
    const piece = bytes.subarray(offset, offset + INPUT_CHUNK);
    if (helper.waiters.length > 0) {
      helper.waiters.shift()(piece);
    } else {
      helper.inputs.push(piece);
    }
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

function frame(command, body) {
  const header = helper.encoder.encode(command + " " + body.length + "\n");
  const bytes = new Uint8Array(header.length + body.length);
  bytes.set(header, 0);
  bytes.set(body, header.length);
  return bytes;
}

// One request at a time. The helper answers in order, so a queue keeps
// each answer with its request.
function send(command, body) {
  const run = () =>
    new Promise((resolve, reject) => {
      helper.pending = { resolve, reject };
      pushInput(frame(command, body));
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

// The support modules: the primitive package of aihc and the small Prelude
// of the guide. The helper compiles them once at the start.
const SUPPORT_FILES = [
  "prim/GHC/Types.hs",
  "prim/GHC/Prim.hs",
  "prim/Aihc/Prim/IO.hs",
  "prim/GHC/Tuple.hs",
  "prim/GHC/CString.hs",
  "prelude/GHC/Classes.hs",
  "prelude/GHC/Num.hs",
  "prelude/GHC/Prim/Base.hs",
  "prelude/Prelude.hs",
];

async function loadSupport() {
  const files = await Promise.all(SUPPORT_FILES.map((path) => fetchText(path)));
  const parts = [];
  files.forEach((text, index) => {
    const body = helper.encoder.encode(text);
    parts.push(helper.encoder.encode(SUPPORT_FILES[index] + " " + body.length + "\n"));
    parts.push(body);
  });
  const total = parts.reduce((sum, part) => sum + part.length, 0);
  const body = new Uint8Array(total);
  let offset = 0;
  for (const part of parts) {
    body.set(part, offset);
    offset += part.length;
  }
  return body;
}

let startPromise = null;

function startHelper() {
  if (startPromise) return startPromise;
  startPromise = (async () => {
    if (typeof WebAssembly === "undefined" || typeof WebAssembly.Suspending !== "function") {
      setHelperState(
        "unsupported",
        "This browser cannot run the playground yet. It needs JavaScript Promise Integration, which Chrome 137, Firefox 139 and Safari 27 have.",
      );
      return;
    }
    setHelperState("loading", "Loading the compiler");
    const started = performance.now();
    try {
      globalThis.lazyReaderHost = { nextInput, write: onStdout, writeErr: onStderr };
      const [module, support] = await Promise.all([import(new URL("helper/aihc-play.js", PLAY_ROOT)), loadSupport()]);
      // The helper runs until the page goes away. Its promise settles only
      // on a failure.
      module.run
        .run()
        .then(() => setHelperState("stopped", "The compiler stopped."))
        .catch((error) => setHelperState("failed", "The compiler failed: " + error));
      setHelperState("loading", "Compiling the Prelude. This takes a few seconds.");
      const answer = await send("load", support);
      if (!answer.ok) throw new Error(answer.message);
      console.info("The Lazy Reader: the compiler is ready after " + Math.round(performance.now() - started) + " ms");
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
  DeclValue: "value declaration",
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
  EParen: "parentheses",
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
  PParen: "parentheses",
  PInfix: "infix pattern",
  PAs: "as-pattern",
  PBang: "bang pattern",
  TyVar: "type variable",
  TyCon: "type constructor",
  TyApp: "type application",
  TyFun: "function type",
  TyList: "list type",
  TyTuple: "tuple type",
  TyParen: "parentheses",
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

function renderTree(node, depth = 0) {
  const row = el("div", { class: "lazy-tree-node", style: "--depth: " + depth });
  row.append(el("span", { class: "lazy-tree-label", text: friendlyLabel(node.label) }));
  if (node.text !== undefined) row.append(el("code", { class: "lazy-tree-text", text: node.text }));
  const item = el("div", { class: "lazy-tree-item" }, [row]);
  if (node.children) {
    const children = el("div", { class: "lazy-tree-children" });
    for (const child of node.children) children.append(renderTree(child, depth + 1));
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
      return "from the Prelude (" + target.module + ")";
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
    autocapitalize: "off",
    autocomplete: "off",
    rows: String(Math.max(3, seed.split("\n").length + 1)),
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
    timer = setTimeout(refresh, 120);
  });
  editor.addEventListener("keydown", (event) => {
    if (event.key === "Tab") {
      event.preventDefault();
      const { selectionStart, selectionEnd, value } = editor;
      editor.value = value.slice(0, selectionStart) + "  " + value.slice(selectionEnd);
      editor.selectionStart = editor.selectionEnd = selectionStart + 2;
      editor.dispatchEvent(new Event("input"));
    }
  });

  helper.listeners.add((state, reason) => {
    if (state === "ready") {
      status.textContent = "";
      refresh();
    } else {
      status.textContent = reason;
    }
  });

  return { refresh };
}

const PANE_TITLES = {
  parse: "The compiler sees",
  names: "The compiler knows",
  types: "The compiler infers",
};

function init() {
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
  roots.forEach((root) => observer.observe(root));
}

// Material loads pages with instant navigation, so the script runs once and
// listens for each page.
if (typeof document$ !== "undefined") {
  document$.subscribe(init);
} else {
  init();
}

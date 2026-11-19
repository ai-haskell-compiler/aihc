// The playgrounds of The Lazy Reader.
//
// Each element with the class "lazy-play" is one playground. Its
// data-command attribute names the helper command: parse, names, or
// types. The seed source is the text of its <pre> child. The script builds
// an editor and an output pane, and asks the helper again after each edit.
//
// The helper is aihc itself, compiled to WebAssembly. It comes from the web
// kit, which a build writes to play/kit/. The page speaks to the kit only
// through its entry module, aihc.js. The helper loads once per page, when
// the first playground scrolls into view.

const KIT = new URL("../play/kit/", import.meta.url);
// Each compile playground adds a function that stops its workers.
const playStops = new Set();

// ---------------------------------------------------------------------------
// The kit

let kitPromise = null;

// Make a compiler. Each compiler starts a worker of its own on its first
// request.
function newCompiler() {
  kitPromise ??= import(new URL("aihc.js", KIT));
  kitPromise.catch(() => {
    kitPromise = null;
  });
  return kitPromise.then(({ createCompiler }) => createCompiler(new URL("manifest.json", KIT)));
}

// The compiler of the parse, names, and types playgrounds.
const helper = {
  state: "idle",
  reason: "",
  compiler: null,
  listeners: new Set(),
};

function setHelperState(state, reason = "") {
  helper.state = state;
  helper.reason = reason;
  for (const listener of helper.listeners) listener(state, reason);
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
      const compiler = await newCompiler();
      await compiler.load();
      helper.compiler = compiler;
      console.info("The Lazy Reader: the compiler is ready " + Math.round(performance.now() - started) + " ms after the start");
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
  return helper.compiler.request(command, source);
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
    let compiler = null, compilerPromise = null, abort = null;
    let activeId = 0, active = false, phase, count;
    let stdout, stderr, warming = null, warm = false;
    const getCompiler = () => {
      compilerPromise ??= newCompiler().then((made) => {
        compiler = made;
        return made;
      });
      compilerPromise.catch(() => {
        compilerPromise = null;
      });
      return compilerPromise;
    };
    playStops.add(() => {
      compiler?.terminate();
      abort?.abort();
    });
    // The first build in a worker is slow. Thus the worker builds the seed
    // once when the playground comes into view. A Run waits for that build
    // and then uses the warm worker.
    const warmUp = () => {
      if (warming) return;
      if (!active) status.textContent = "Download and start the compiler.";
      warming = getCompiler()
        .then((made) => made.warm(seed))
        .then(
          () => {
            warm = true;
            if (!active) status.textContent = "Select Run to compile the module.";
          },
          (error) => {
            warming = null;
            if (!active && error?.name !== "AbortError") status.textContent = String(error?.message ?? error);
          },
        );
    };
    // The playground downloads tens of megabytes. Thus it stays locked until
    // the reader enables it. If the browser has all the files from an earlier
    // visit, the playground starts without the question.
    const panes = [...root.querySelectorAll(".lazy-pane")];
    const enable = el("button", { type: "button", class: "lazy-run lazy-enable", text: "Enable the compiler in this browser" });
    const note = el("p", { class: "lazy-consent-note", text: "Find the download size." });
    const consent = el("div", { class: "lazy-consent" }, [enable, note]);
    root.classList.add("lazy-locked");
    for (const pane of panes) pane.inert = true;
    root.append(consent);
    const unlock = () => {
      root.classList.remove("lazy-locked");
      for (const pane of panes) pane.inert = false;
      consent.remove();
      status.textContent = "Select Run to compile the module.";
    };
    enable.addEventListener("click", () => {
      unlock();
      warmUp();
    });
    getCompiler()
      .then((made) => made.downloadSize())
      .then(({ missing }) => {
        if (missing > 0) {
          const size = Math.max(1, Math.round(missing / 1e6)) + " MB";
          enable.textContent = "Enable the compiler in this browser (" + size + ")";
          note.textContent =
            "The compiler and the linker run in your browser. This button downloads " + size +
            " of files. The browser keeps the files for your next visit.";
          return;
        }
        unlock();
        // The first build in a worker is slow, so do it before the first Run.
        const visibility = new IntersectionObserver((entries) => {
          if (entries.some((entry) => entry.isIntersecting)) {
            visibility.disconnect();
            warmUp();
          }
        });
        visibility.observe(root);
      })
      .catch(() => {
        note.textContent = "The compiler and the linker run in your browser. This button downloads their files.";
      });
    const finish = () => {
      active = false;
      abort = null;
      run.disabled = false;
      stop.disabled = true;
    };
    const cancel = () => {
      // A synchronous compiler operation needs worker termination to stop.
      if (phase === "build") {
        compiler?.terminate();
        warm = false;
        warming = null;
      }
      abort?.abort();
      finish();
    };
    run.addEventListener("click", async () => {
      output.textContent = "";
      stdout = el("pre", { class: "lazy-program-stdout" });
      stderr = el("pre", { class: "lazy-program-stderr" });
      output.append(stdout, stderr);
      count = 0;
      active = true;
      const id = ++activeId;
      phase = "build";
      abort = new AbortController();
      const { signal } = abort;
      run.disabled = true;
      stop.disabled = false;
      const current = () => active && activeId === id;
      const setStatus = (text) => {
        if (current()) status.textContent = text;
      };
      const append = (pane) => (text) => {
        if (!current()) return;
        pane.append(document.createTextNode(text.slice(0, Math.max(0, 65536 - count))));
        count += text.length;
        if (count > 65536) {
          status.textContent = "The output reached the size limit. The program stopped.";
          cancel();
        }
      };
      status.textContent = warm ? "Compile the module." : "Load the compiler.";
      try {
        const made = await getCompiler();
        const built = await made.compile(editor.value, { onStatus: setStatus, onLog: append(stderr) });
        if (!current()) return;
        if (!built.ok) {
          output.append(problemsList(built.diagnostics));
          setStatus("Correct the module, then select Run.");
          finish();
          return;
        }
        phase = "program";
        setStatus("Run the program.");
        const { exitCode } = await made.run(built.program, { signal, onStdout: append(stdout), onStderr: append(stderr) });
        if (!current()) return;
        setStatus(exitCode === 0 ? "The program finished." : "The program exited with code " + exitCode + ".");
        finish();
      } catch (error) {
        if (!current()) return;
        setStatus(String(error?.message ?? error));
        // A failed build can leave the worker in an unknown state.
        if (phase === "build") {
          compiler?.terminate();
          warm = false;
          warming = null;
        }
        finish();
      }
    });
    stop.addEventListener("click", () => {
      cancel();
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

  // Update the answer only when the reader stops typing.
  editor.addEventListener("input", () => {
    editor.rows = Math.max(3, editor.value.split("\n").length + 1);
    clearTimeout(timer);
    if (command !== "compile") timer = setTimeout(refresh, TYPING_PAUSE_MS);
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

// The time without a keystroke before a playground asks the helper again.
const TYPING_PAUSE_MS = 1000;

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
  for (const stop of playStops) stop();
  playStops.clear();
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

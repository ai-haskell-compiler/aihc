// Interactive info table examples for the "Info tables" page.
//
// Each element with the class "info-explorer" becomes one example. The
// data-kind attribute selects "function", "constructor", or "thunk". The
// data-args attribute gives the initial argument representations, for
// example "lifted int lifted".
//
// The builders below copy the rules of Aihc.Lir.Lower and aihc_runtime.c.
// Each builder returns plain data: tables, the current table, and the heap
// object. The renderer reads only this data. Thus, generated JSON from the
// compiler can replace the builders later.
(function () {
  "use strict";

  const REPS = {
    lifted: { label: "a", title: "Lifted value", slots: [{ pointer: true, rep: "a" }] },
    int: { label: "Int#", title: "Int# value", slots: [{ pointer: false, rep: "Int#" }] },
    pair: {
      label: "(# a, Int# #)",
      title: "Unboxed pair: two fields",
      slots: [
        { pointer: true, rep: "a" },
        { pointer: false, rep: "Int#" },
      ],
    },
  };

  const KINDS = {
    function: { reps: ["lifted", "int", "pair"], min: 1, max: 6 },
    constructor: { reps: ["lifted", "int"], min: 0, max: 6 },
    thunk: { reps: ["lifted", "int"], min: 1, max: 6 },
  };

  // grinApplyGroupLimit in Aihc.Grin.Syntax.
  const APPLY_GROUP_LIMIT = 4;

  const TARGETS = {
    "64-bit": { word: 8, size: 48 },
    wasm32: { word: 4, size: 28 },
  };

  const FIELDS = [
    "identity",
    "field_is_pointer",
    "next",
    "backend_entry",
    "srt",
    "field_count",
    "remaining_arity",
    "frame_kind",
    "object_kind",
    "needs_eval",
  ];

  const THUNK_STATES = ["Not evaluated", "Under evaluation", "Updated"];

  // Five word fields, then five byte fields. See struct AihcInfo.
  function fieldOffset(index, target) {
    const word = TARGETS[target].word;
    return index < 5 ? index * word : 5 * word + (index - 5);
  }

  function argSlots(reps, count) {
    const slots = [];
    reps.slice(0, count).forEach((rep, index) => {
      const name = "x" + (index + 1);
      const repSlots = REPS[rep].slots;
      repSlots.forEach((slot) => {
        slots.push({
          label: name,
          detail: repSlots.length > 1 ? slot.rep : null,
          pointer: slot.pointer,
          arg: index,
        });
      });
    });
    return slots;
  }

  function pointerMap(slots) {
    return "[" + slots.map((slot) => (slot.pointer ? "1" : "0")).join(", ") + "]";
  }

  function plural(count, word) {
    return count + " " + word + (count === 1 ? "" : "s");
  }

  function signature(reps, result) {
    return reps
      .map((rep) => REPS[rep].label)
      .concat([result])
      .join(" -> ");
  }

  function application(head, count) {
    const args = [];
    for (let index = 1; index <= count; index++) args.push("x" + index);
    return [head].concat(args).join(" ");
  }

  function buildFunction(reps, supplied) {
    const arity = reps.length;
    const tables = [];
    for (let stage = 0; stage <= arity; stage++) {
      const slots = argSlots(reps, stage);
      const remaining = arity - stage;
      let entry;
      if (remaining === 0) entry = "null";
      else if (remaining > APPLY_GROUP_LIMIT) entry = "null (over " + APPLY_GROUP_LIMIT + " groups)";
      else entry = "adapter: " + stage + " stored + " + remaining + " supplied";
      tables.push({
        id: "stage" + stage,
        title: "f",
        subtitle: stage + " supplied",
        values: {
          identity: "code of f",
          field_is_pointer: pointerMap(slots),
          next: stage < arity ? "f, " + (stage + 1) + " supplied" : "null",
          backend_entry: entry,
          srt: "SRT of f, or null",
          field_count: String(slots.length),
          remaining_arity: String(remaining),
          frame_kind: "0 (none)",
          object_kind: "1 (closure)",
          needs_eval: "0 (value)",
        },
      });
    }

    const source = ["f :: " + signature(reps, "r"), "", "value = " + application("f", supplied)];
    if (supplied === arity) {
      return {
        source: source,
        tables: tables,
        current: null,
        object: null,
        note:
          "This application is saturated. The code calls f directly and makes no closure. " +
          "The last table has no next value and no backend entry.",
      };
    }
    const slots = argSlots(reps, supplied).map((slot) => ({
      label: slot.label,
      detail: slot.detail,
      pointer: slot.pointer,
      fresh: slot.arg === supplied - 1,
    }));
    const added = slots.filter((slot) => slot.fresh).length;
    let note;
    if (supplied === 0) {
      note = "The function value f has no stored fields. Its table requires " + plural(arity, "argument") + ".";
    } else if (slots.length === added) {
      note =
        "The application makes a new object with " +
        plural(added, "field") +
        ". The header points to the table for 1 supplied argument.";
    } else {
      note =
        "The application copies " +
        plural(slots.length - added, "stored field") +
        " from the previous object and adds " +
        plural(added, "new field") +
        ". The header points to the next table in the chain.";
    }
    if (arity - supplied > APPLY_GROUP_LIMIT) {
      note +=
        " More than " +
        APPLY_GROUP_LIMIT +
        " argument groups remain, so this table has no backend entry. One application cannot supply all of them.";
    }
    return {
      source: source,
      tables: tables,
      current: "stage" + supplied,
      object: { header: "stage" + supplied, headerNote: null, slots: slots },
      note: note,
    };
  }

  function buildConstructor(reps, supplied) {
    const arity = reps.length;
    const all = argSlots(reps, arity);
    const tables = [];
    if (arity > 0) {
      tables.push({
        id: "partial",
        title: "T",
        subtitle: "partial",
        values: {
          identity: "T saturated table",
          field_is_pointer: pointerMap(all),
          next: "T saturated",
          backend_entry: "null",
          srt: "null",
          field_count: String(all.length),
          remaining_arity: String(arity),
          frame_kind: "0 (none)",
          object_kind: "3 (partial constructor)",
          needs_eval: "0 (value)",
        },
      });
    }
    tables.push({
      id: "saturated",
      title: "T",
      subtitle: "saturated",
      values: {
        identity: "T saturated table",
        field_is_pointer: pointerMap(all),
        next: "null",
        backend_entry: "null",
        srt: "null",
        field_count: String(all.length),
        remaining_arity: "0",
        frame_kind: "0 (none)",
        object_kind: "0 (node)",
        needs_eval: "0 (value)",
      },
    });

    const declaration = ["data T = T"].concat(reps.map((rep) => REPS[rep].label)).join(" ");
    const source = [declaration, "", "value = " + application("T", supplied)];
    const stored = argSlots(reps, supplied).map((slot) => ({
      label: slot.label,
      detail: slot.detail,
      pointer: slot.pointer,
      fresh: slot.arg === supplied - 1,
    }));

    if (supplied === arity) {
      return {
        source: source,
        tables: tables,
        current: "saturated",
        object: { header: "saturated", headerNote: null, slots: stored },
        note:
          arity === 0
            ? "A nullary constructor has only the saturated table. The backend also makes a static object for it."
            : "The last argument makes a saturated node. The node drops the stored count and uses the saturated table.",
      };
    }
    const count = { label: "count", detail: String(supplied), pointer: false, count: true };
    return {
      source: source,
      tables: tables,
      current: "partial",
      object: { header: "partial", headerNote: null, slots: [count].concat(stored) },
      note:
        "All partial stages share one table. Field 0 of the object stores the count of the filled fields. " +
        "The pointer map of the saturated table also applies to this prefix of fields.",
    };
  }

  function buildThunk(reps, state) {
    const captured = reps.length;
    const slots = argSlots(reps, captured);
    const tables = [
      {
        id: "thunk",
        title: "t",
        subtitle: "thunk",
        values: {
          identity: "code of t",
          field_is_pointer: pointerMap(slots),
          next: "null",
          backend_entry: "adapter: " + captured + " stored",
          srt: "SRT of t, or null",
          field_count: String(slots.length),
          remaining_arity: "0",
          frame_kind: "0 (none)",
          object_kind: "2 (thunk)",
          needs_eval: "1 (enter)",
        },
      },
      {
        id: "indirection",
        title: "indirection",
        subtitle: "run-time system",
        values: {
          identity: "0",
          field_is_pointer: "[1]",
          next: "null",
          backend_entry: "null",
          srt: "null",
          field_count: "1",
          remaining_arity: "0",
          frame_kind: "0 (none)",
          object_kind: "4 (indirection)",
          needs_eval: "2 (follow)",
        },
      },
    ];
    const source = ["t = " + application("work", captured)];
    const plain = slots.map((slot) => ({ label: slot.label, detail: slot.detail, pointer: slot.pointer }));

    if (state === 0) {
      return {
        source: source,
        tables: tables,
        current: "thunk",
        object: { header: "thunk", headerNote: null, slots: plain },
        note:
          "The thunk stores the captured values. The inline evaluation check reads needs_eval = 1 " +
          "and calls the run-time evaluation code.",
      };
    }
    if (state === 1) {
      return {
        source: source,
        tables: tables,
        current: "thunk",
        object: { header: "thunk", headerNote: "bit 0 set: evaluating", slots: plain },
        note:
          "The run-time system pushes an update frame and sets bit 0 of the header. The header still points to the thunk table. " +
          "The object is now a blackhole: a second evaluation waits or fails.",
      };
    }
    const updated = [{ label: "result", detail: null, pointer: true, fresh: true }].concat(
      plain.slice(1).map((slot) => Object.assign({}, slot, { dead: true })),
    );
    return {
      source: source,
      tables: tables,
      current: "indirection",
      object: { header: "indirection", headerNote: null, slots: updated },
      note:
        "The update writes the result into field 0 and changes the header to the indirection table. " +
        "Compiled code reads needs_eval = 2 and follows field 0 without a call.",
    };
  }

  const BUILDERS = { function: buildFunction, constructor: buildConstructor, thunk: buildThunk };

  function el(tag, attrs, children) {
    const node = document.createElement(tag);
    Object.entries(attrs || {}).forEach(([key, value]) => {
      if (value === null || value === undefined || value === false) return;
      if (key === "class") node.className = value;
      else if (key === "text") node.textContent = value;
      else if (key.startsWith("on")) node.addEventListener(key.slice(2), value);
      else node.setAttribute(key, value === true ? "" : value);
    });
    (children || []).forEach((child) => {
      if (child === null || child === undefined) return;
      node.appendChild(typeof child === "string" ? document.createTextNode(child) : child);
    });
    return node;
  }

  function setup(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "true";
    const kind = KINDS[root.dataset.kind] ? root.dataset.kind : "function";
    const limits = KINDS[kind];
    let reps = (root.dataset.args || "lifted int lifted")
      .split(/\s+/)
      .filter((rep) => limits.reps.includes(rep))
      .slice(0, limits.max);
    while (reps.length < limits.min) reps.push("lifted");
    const state = {
      reps: reps,
      step: kind === "thunk" ? 0 : Math.min(1, reps.length),
      target: "64-bit",
      previous: null,
    };

    root.replaceChildren();
    root.classList.add("info-explorer--ready");

    const argsBox = el("div", { class: "ix-args" });
    const stepBox = el("div", { class: "ix-step" });
    const targetSelect = el(
      "select",
      {
        class: "ix-select",
        "aria-label": "Target",
        onchange: (event) => {
          state.target = event.target.value;
          renderOutput();
        },
      },
      Object.keys(TARGETS).map((name) => el("option", { value: name, text: name })),
    );
    const controls = el("div", { class: "ix-controls" }, [
      argsBox,
      el("div", { class: "ix-row" }, [stepBox, el("label", { class: "ix-target" }, ["Target ", targetSelect])]),
    ]);
    const output = el("div", { class: "ix-output", "aria-live": "polite" });
    root.append(controls, output);

    function renderArgs() {
      const label = kind === "thunk" ? "Captured values" : "Arguments";
      const chips = state.reps.map((rep, index) =>
        el("label", { class: "ix-chip" }, [
          el("span", { class: "ix-chip-name", text: "x" + (index + 1) }),
          el(
            "select",
            {
              class: "ix-select",
              "aria-label": "Representation of x" + (index + 1),
              onchange: (event) => {
                state.reps[index] = event.target.value;
                renderOutput();
              },
            },
            limits.reps.map((name) =>
              el("option", { value: name, selected: name === rep, text: REPS[name].label, title: REPS[name].title }),
            ),
          ),
        ]),
      );
      const remove = el("button", {
        type: "button",
        class: "ix-button",
        text: "−",
        title: "Remove the last one",
        "aria-label": "Remove the last " + (kind === "thunk" ? "captured value" : "argument"),
        disabled: state.reps.length <= limits.min,
        onclick: () => {
          state.reps.pop();
          state.step = Math.min(state.step, kind === "thunk" ? 2 : state.reps.length);
          renderArgs();
          renderStep();
          renderOutput();
        },
      });
      const add = el("button", {
        type: "button",
        class: "ix-button",
        text: "+",
        title: "Add one",
        "aria-label": "Add " + (kind === "thunk" ? "a captured value" : "an argument"),
        disabled: state.reps.length >= limits.max,
        onclick: () => {
          state.reps.push(limits.reps[state.reps.length % limits.reps.length]);
          renderArgs();
          renderStep();
          renderOutput();
        },
      });
      argsBox.replaceChildren(
        el("span", { class: "ix-label", text: label }),
        el("div", { class: "ix-chips" }, chips.concat([el("span", { class: "ix-buttons" }, [remove, add])])),
      );
    }

    function renderStep() {
      if (kind === "thunk") {
        stepBox.replaceChildren(
          el("span", { class: "ix-label", text: "State" }),
          el(
            "div",
            { class: "ix-segments", role: "group", "aria-label": "Thunk state" },
            THUNK_STATES.map((name, index) =>
              el("button", {
                type: "button",
                class: "ix-segment",
                "aria-pressed": index === state.step ? "true" : "false",
                text: name,
                onclick: () => {
                  state.step = index;
                  renderStep();
                  renderOutput();
                },
              }),
            ),
          ),
        );
        return;
      }
      const arity = state.reps.length;
      const readout = el("output", { class: "ix-readout", text: state.step + " of " + arity });
      const slider = el("input", {
        type: "range",
        class: "ix-slider",
        min: "0",
        max: String(arity),
        value: String(state.step),
        "aria-label": "Supplied arguments",
        disabled: arity === 0,
        oninput: (event) => {
          state.step = Number(event.target.value);
          readout.textContent = state.step + " of " + arity;
          renderOutput();
        },
      });
      stepBox.replaceChildren(el("span", { class: "ix-label", text: "Supplied" }), slider, readout);
    }

    function renderObject(model) {
      if (!model.object) {
        return el("div", { class: "ix-object ix-object--none" }, [
          el("div", { class: "ix-heading", text: "Heap object" }),
          el("div", { class: "ix-empty", text: "No closure. The code calls f." }),
        ]);
      }
      const header = model.tables.find((table) => table.id === model.object.header);
      const cells = [
        el("div", { class: "ix-cell ix-cell--header" + (model.object.headerNote ? " ix-cell--tagged" : "") }, [
          el("span", { class: "ix-cell-index", text: "header" }),
          el("span", { class: "ix-cell-label", text: "→ " + header.title + ", " + header.subtitle }),
          model.object.headerNote ? el("span", { class: "ix-cell-detail", text: model.object.headerNote }) : null,
        ]),
      ].concat(
        model.object.slots.map((slot, index) =>
          el(
            "div",
            {
              class:
                "ix-cell" +
                (slot.pointer ? " ix-cell--pointer" : "") +
                (slot.count ? " ix-cell--count" : "") +
                (slot.fresh ? " ix-cell--fresh" : "") +
                (slot.dead ? " ix-cell--dead" : ""),
              title: slot.dead ? "Not read after the update" : slot.pointer ? "Managed pointer" : "Not a pointer",
            },
            [
              el("span", { class: "ix-cell-index", text: "field " + index }),
              el("span", { class: "ix-cell-label", text: slot.label }),
              slot.detail ? el("span", { class: "ix-cell-detail", text: slot.detail }) : null,
            ],
          ),
        ),
      );
      const words = 1 + model.object.slots.length;
      return el("div", { class: "ix-object" }, [
        el("div", { class: "ix-heading", text: "Heap object: " + plural(words, "word") }),
        el("div", { class: "ix-cells" }, cells),
      ]);
    }

    function renderTable(table, model, previous) {
      const current = table.id === model.current;
      const rows = FIELDS.map((field, index) => {
        const value = table.values[field];
        const changed = previous && previous.current !== model.current && current;
        return el("tr", {}, [
          el("td", { class: "ix-offset", text: String(fieldOffset(index, state.target)) }),
          el("td", { class: "ix-field" }, [el("code", { text: field })]),
          el("td", { class: "ix-value" + (changed ? " ix-value--changed" : ""), text: value }),
        ]);
      });
      return el("figure", { class: "ix-table" + (current ? " ix-table--current" : "") }, [
        el("figcaption", {}, [
          el("span", { class: "ix-table-title", text: table.title }),
          el("span", { class: "ix-table-subtitle", text: table.subtitle }),
          current ? el("span", { class: "ix-badge", text: "header" }) : null,
        ]),
        el("table", { class: "ix-grid" }, [
          el("thead", {}, [
            el("tr", {}, [el("th", { text: "Offset" }), el("th", { text: "Field" }), el("th", { text: "Value" })]),
          ]),
          el("tbody", {}, rows),
        ]),
      ]);
    }

    function renderOutput() {
      const model = BUILDERS[kind](state.reps, state.step);
      const chain = [];
      model.tables.forEach((table, index) => {
        if (index > 0) {
          const linked = model.tables[index - 1].values.next !== "null";
          chain.push(
            el("div", {
              class: "ix-link" + (linked ? "" : " ix-link--none"),
              "aria-hidden": "true",
              text: linked ? "next →" : "",
            }),
          );
        }
        chain.push(renderTable(table, model, state.previous));
      });
      const current = model.tables.find((table) => table.id === model.current);
      if (current) {
        const scrollTarget = () => {
          const node = output.querySelector(".ix-table--current");
          const box = output.querySelector(".ix-chain");
          if (node && box) box.scrollLeft = Math.max(0, node.offsetLeft - 16);
        };
        requestAnimationFrame(scrollTarget);
      }
      output.replaceChildren(
        el("pre", { class: "ix-source" }, [el("code", { text: model.source.join("\n") })]),
        renderObject(model),
        el("p", { class: "ix-note", text: model.note }),
        el("div", { class: "ix-heading", text: "Info tables (" + TARGETS[state.target].size + " bytes each)" }),
        el("div", { class: "ix-chain" }, chain),
      );
      state.previous = model;
    }

    renderArgs();
    renderStep();
    renderOutput();
  }

  function setupAll() {
    document.querySelectorAll(".info-explorer").forEach(setup);
  }

  // Material for MkDocs gives document$ for instant page loads.
  if (typeof window.document$ !== "undefined") window.document$.subscribe(setupAll);
  else if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", setupAll);
  else setupAll();
})();

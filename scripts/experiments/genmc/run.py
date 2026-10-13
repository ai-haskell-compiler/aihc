#!/usr/bin/env python3
"""Run the GenMC RTS feasibility probes. Keep results outside the source tree."""

import argparse
import json
import os
from pathlib import Path
import subprocess


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
RTS = ROOT / "core-libs/aihc-rts/native"
SOURCES = ("aihc_runtime", "aihc_gc", "aihc_heap_regions", "aihc_host_posix")
ENGINE = [
    "genmc",
    "-disable-function-inliner",
    "-disable-estimation",
    "-disable-mm-detector",
    "-print-error-trace",
]


def command(arguments, log):
    """Keep the command, output, and status for each probe."""
    with log.open("w") as stream:
        result = subprocess.run(
            arguments, stdout=stream, stderr=subprocess.STDOUT, timeout=60
        )
    return result.returncode


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    include = os.environ["GENMC_INCLUDE"]
    manifest = {
        "revision": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
        ).strip(),
        "genmc_revision": "9f6c4c0772d0c42b325681581acc6a0f3eb9b5f7",
        "commands": [],
        "results": [],
    }

    def run(arguments, name):
        manifest["commands"].append(arguments)
        code = command(arguments, output / f"{name}.log")
        return code

    def compile_c(source, name, flags=()):
        destination = output / f"{name}.ll"
        arguments = [
            "clang", "-g", "-O0", "-Xclang", "-disable-O0-optnone",
            "-fno-discard-value-names", "-fcommon",
            f"-I{include}", f"-I{RTS}", "-include", str(HERE / "declarations.h"),
            *flags, "-S", "-emit-llvm", str(source), "-o", str(destination),
        ]
        if run(arguments, f"{name}-compile") != 0:
            raise RuntimeError(f"Compilation failed: {source}")
        return str(destination)

    try:
        runtime = {
            name: compile_c(RTS / f"{name}.c", name) for name in SOURCES
        }
        probes = [
            ("counter-safe", "counter", SOURCES[:1], (), 0, "No errors were detected."),
            ("counter-race", "counter", SOURCES[:1], ("-DINJECT_RACE",), 42, "Non-atomic race!"),
            ("mvar-safe", "mvar", SOURCES[:3], (), 0, "No errors were detected."),
            ("mvar-race", "mvar", SOURCES[:3], ("-DINJECT_RACE",), 42, "Non-atomic race!"),
            ("startup", "startup", SOURCES, (), 17, "unknown external function: aihc_rts_nursery_bytes"),
            ("host-map", "host-map", SOURCES[3:], (), 17, "mmap"),
            ("host-clock", "host-clock", SOURCES[3:], (), 17, "unknown external function: clock_gettime"),
            ("old-barrier", "old-barrier", SOURCES[:3], (), 17, "unknown external function: realloc"),
            ("pinned", "pinned", SOURCES[:3], (), 17, "unknown external function: calloc"),
        ]
        for name, source, units, flags, expected_code, expected_text in probes:
            driver = compile_c(HERE / f"{source}.c", name, flags)
            linked = output / f"{name}-linked.ll"
            code = run(
                ["llvm-link", "-S", driver, *(runtime[unit] for unit in units),
                 "-o", str(linked)], f"{name}-link"
            )
            if code != 0:
                raise RuntimeError(f"LLVM link failed: {name}")
            module = output / f"{name}-ready.ll"
            code = run(
                ["opt", "-S", "-passes=internalize,globaldce",
                 "-internalize-public-api-list=main", str(linked), "-o", str(module)],
                f"{name}-prune",
            )
            if code != 0:
                raise RuntimeError(f"LLVM preparation failed: {name}")
            code = run(
                [*ENGINE, f"-dump-error-graph={output / (name + '.dot')}", str(module)],
                name,
            )
            text = (output / f"{name}.log").read_text()
            matched = code == expected_code and expected_text in text
            if name.endswith("-race"):
                matched = matched and "aihc_runtime.c:" in text
            manifest["results"].append(
                {"probe": name, "exit": code, "expected": matched}
            )
            print(f"{name}: exit {code}, expected result {matched}", flush=True)
            if not matched:
                raise RuntimeError(f"Unexpected result: {name}")

        traces = []
        for repeat in (1, 2):
            name = f"replay-{repeat}"
            code = run(
                [*ENGINE, "-schedule-policy=arbitrary", "-schedule-seed=17",
                 "-print-schedule-seed", str(output / "mvar-race-ready.ll")], name
            )
            text = (output / f"{name}.log").read_text()
            if code != 42 or "Non-atomic race!" not in text:
                raise RuntimeError(f"Replay did not report the race: {name}")
            traces.append("\n".join(
                line for line in text.splitlines()
                if not line.startswith("Total wall-clock time:")
            ))
        if traces[0] != traces[1]:
            raise RuntimeError("Replay traces differ")
        manifest["replay_equal"] = True
        print("Replay traces match after removal of elapsed time.", flush=True)
        code = run([*ENGINE, str(output / "mvar-safe-ready.ll")], "mvar-restored")
        if code != 0 or "No errors were detected." not in (
            output / "mvar-restored.log"
        ).read_text():
            raise RuntimeError("The restored control case failed")
        manifest["restored_control_passed"] = True
        print("The restored MVar control case passed.", flush=True)
    finally:
        (output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
    main()

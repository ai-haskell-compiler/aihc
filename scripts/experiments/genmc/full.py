#!/usr/bin/env python3
"""Link the C and Lir RTS. Compare GenMC results with native execution."""

import argparse
import json
import os
from pathlib import Path
import signal
import subprocess
import sys

sys.dont_write_bytecode = True

from run import ENGINE, HERE, ROOT, RTS, SOURCES


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--timeout", type=int, default=60)
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    manifest = {
        "revision": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
        ).strip(),
        "genmc_revision": "9f6c4c0772d0c42b325681581acc6a0f3eb9b5f7",
        "commands": [],
        "results": [],
    }

    def run(arguments, name):
        record = {"name": name, "arguments": arguments}
        manifest["commands"].append(record)
        with (output / f"{name}.log").open("w") as log:
            try:
                result = subprocess.run(
                    arguments, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT,
                    timeout=args.timeout,
                )
                record["exit"] = result.returncode
            except subprocess.TimeoutExpired:
                record["exit"] = "timeout"
        return record["exit"]

    def require(arguments, name):
        if run(arguments, name) != 0:
            raise RuntimeError(f"Command failed: {name}. See its log.")

    def compile_c(source, name, native=False, flags=(), library=False):
        destination = output / (name + (".o" if native else ".ll"))
        includes = [] if native else [f'-I{os.environ["GENMC_INCLUDE"]}']
        replacement = [] if library else ["-include", str(HERE / "libc.h")]
        arguments = [
            "clang", "-g", "-O0", "-fno-builtin", "-fcommon",
            "-Xclang", "-disable-O0-optnone", "-fno-discard-value-names",
            f"-I{RTS}", *includes, *replacement, *flags,
            *(["-c"] if native else ["-S", "-emit-llvm"]),
            str(source), "-o", str(destination),
        ]
        require(arguments, name + "-compile")
        return str(destination)

    try:
        lir = output / "rts.ll"
        require(
            ["cabal", "exec", "--", "runghc", "-pgmc=clang", "-pgml=clang", "-package=aihc",
             str(HERE / "emit-lir.hs"), str(RTS / "rts.lir"), str(lir)],
            "emit-lir",
        )
        units = {}
        for native in (False, True):
            suffix = "-native" if native else ""
            compiled = {
                name: compile_c(RTS / f"{name}.c", name + suffix, native)
                for name in SOURCES
            }
            compiled["allocated-host"] = compile_c(
                RTS / "aihc_host_posix.c", "allocated-host" + suffix, native,
                ("-Daihc_host_map_regions=aihc_posix_map_regions",
                 "-Daihc_host_unmap_regions=aihc_posix_unmap_regions"),
            )
            compiled["libc"] = compile_c(
                HERE / "libc.c", "libc" + suffix, native, library=True
            )
            compiled["regions"] = compile_c(
                HERE / "regions.c", "region-allocator" + suffix, native
            )
            if native:
                compiled["lir"] = str(output / "rts-native.o")
                require(["clang", "-c", str(lir), "-o", compiled["lir"]], "lir-native")
            else:
                compiled["lir"] = str(lir)
            units[native] = compiled

        # None records a resource observation. It never means verification passed.
        probes = [
            ("libc-bytes", "libc-bytes", (), False, 0, "No errors were detected."),
            ("libc-zero-word", "libc-zero-word", (), False, 0, "No errors were detected."),
            ("libc-copy-word", "libc-copy-word", (), False, -signal.SIGABRT, "INTERNAL FAILURE"),
            ("lir", "lir", (), False, 0, "No errors were detected."),
            ("old-barrier", "old-barrier", (), False, 0, "No errors were detected."),
            ("pinned", "pinned", (), False, 0, "No errors were detected."),
            ("mvar-safe", "mvar", (), False, 0, "No errors were detected."),
            ("mvar-race", "mvar", ("-DINJECT_RACE",), False, 42, "Non-atomic race!"),
            ("regions", "regions-control", (), True, 0, "No errors were detected."),
            ("startup-posix", "machine", (), False, 17, "mmap"),
            ("host-clock", "host-clock", (), False, 17, "clock_gettime"),
            ("startup-allocated", "machine", (), True, None, ""),
            ("gc-allocated", "machine", ("-DCOLLECT",), True, None, ""),
        ]
        for name, source, flags, allocated, expected_code, expected_text in probes:
            for native in (True, False):
                # Native data races are undefined behavior. Only GenMC runs this fault.
                if native and name.endswith("-race"):
                    continue
                suffix = "-native" if native else ""
                label = name + suffix
                driver = compile_c(HERE / f"{source}.c", label, native, flags)
                compiled = units[native]
                host = "allocated-host" if allocated else "aihc_host_posix"
                inputs = [
                    driver, *(compiled[unit] for unit in SOURCES[:3]),
                    compiled[host], compiled["libc"], compiled["lir"],
                ]
                if allocated:
                    inputs.append(compiled["regions"])
                if native:
                    executable = str(output / label)
                    require(["clang", "-pthread", *inputs, "-o", executable], label + "-link")
                    require([executable], label)
                    manifest["results"].append({"probe": label, "exit": 0, "status": "passed"})
                    print(f"{label}: passed", flush=True)
                    continue
                linked = output / f"{name}-linked.ll"
                module = output / f"{name}-ready.ll"
                require(["llvm-link", "-S", *inputs, "-o", str(linked)], name + "-link")
                require(
                    ["opt", "-S", "-passes=internalize,globaldce",
                     "-internalize-public-api-list=main", str(linked), "-o", str(module)],
                    name + "-prune",
                )
                code = run([*ENGINE, str(module)], name)
                text = (output / f"{name}.log").read_text()
                matched = code == expected_code and expected_text in text
                if name.endswith("-race"):
                    matched = matched and "aihc_runtime.c:" in text
                status = "passed" if code == 0 and "No errors were detected." in text else "blocked"
                if name.endswith("-race") and matched:
                    status = "race detected"
                if code == "timeout":
                    status = "incomplete"
                manifest["results"].append(
                    {"probe": name, "exit": code, "status": status,
                     "matches_prior_result": matched if expected_code is not None else None}
                )
                print(f"{name}: {status}, exit {code}", flush=True)
                if expected_code is not None and not matched:
                    raise RuntimeError(f"Unexpected result: {name}. See its log.")
    finally:
        (output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
    main()

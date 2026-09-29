"""Highlight the AIHC intermediate languages in fenced code blocks.

A fence with the language aihc-fc, aihc-grin, or aihc-lir goes through the
highlighter of editors/grammars. That highlighter uses the same TextMate
grammars as the editor support. It fails when a grammar does not recognize a
token, and then the build fails.

AIHC_HIGHLIGHT is the highlighter command. The default is aihc-highlight on
the PATH. `nix build .#aihc-grammars` builds it.
"""

import os
import shlex
import subprocess

from pymdownx.superfences import SuperFencesException

LANGUAGES = {"aihc-fc": "fc", "aihc-grin": "grin", "aihc-lir": "lir"}


def _highlight(source, language, class_name, options, md, **kwargs):
    command = shlex.split(os.environ.get("AIHC_HIGHLIGHT", "aihc-highlight"))
    try:
        result = subprocess.run(
            [*command, LANGUAGES[language]],
            input=source,
            capture_output=True,
            text=True,
            check=False,
        )
    except OSError as error:
        # SuperFences shows a fence as plain text after any other exception.
        raise SuperFencesException(f"Cannot run {command[0]}: {error}") from error
    if result.returncode != 0:
        raise SuperFencesException(f"{language}: {result.stderr.strip()}")
    return result.stdout


def on_config(config):
    superfences = config.mdx_configs.setdefault("pymdownx.superfences", {})
    fences = superfences.setdefault("custom_fences", [])
    fences.extend({"name": name, "class": "highlight", "format": _highlight} for name in LANGUAGES)
    return config

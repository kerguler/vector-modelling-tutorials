#!/usr/bin/env python3
import sys, html, runpy

# ---- minimal compatibility shim for removed stdlib module ----
try:
    import cgi
except ModuleNotFoundError:
    import types
    cgi = types.SimpleNamespace()
setattr(cgi, "escape", html.escape)

# ---- run nbviewer with all arguments ----
sys.argv = [
    "nbviewer",
    "--localfiles=/srv/tutorials",
    "--localfile-path=/srv/tutorials",
    "--base-url=/tutorials-viewer",
    "--port=80",
    "--no-cache",
    "--jupyterhub-url=https://veclim.com/tutorials",
]

runpy.run_module("nbviewer", run_name="__main__")

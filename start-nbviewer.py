#!/usr/bin/env python3
import cgi, html, runpy, sys

# ---- Compatibility shim ----
setattr(cgi, "escape", html.escape)

# ---- Construct the arguments ----
sys.argv = [
    "nbviewer",
    "--localfiles=/srv/tutorials",
    "--localfile-path=/srv/tutorials",
    "--base-url=/tutorials-viewer",
    "--port=80",
    "--no-cache",
    "--jupyterhub-url=https://veclim.com/tutorials",
]

# ---- Run nbviewer ----
runpy.run_module("nbviewer", run_name="__main__")

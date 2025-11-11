#!/usr/bin/env python3
import sys, types, html, runpy

# ---- Provide a fake 'cgi' module before nbviewer imports it ----
fake_cgi = types.ModuleType("cgi")
fake_cgi.escape = html.escape
sys.modules["cgi"] = fake_cgi

# ---- All CLI arguments for nbviewer ----
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

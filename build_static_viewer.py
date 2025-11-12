import pathlib, subprocess

tutorial_root = pathlib.Path("./tutorials")
output_root   = pathlib.Path("./tutorials-html")

# Find all notebooks
for nb in tutorial_root.rglob("*.ipynb"):
    rel = nb.relative_to(tutorial_root)
    outdir = (output_root / rel.parent)
    outdir.mkdir(parents=True, exist_ok=True)

    print(f"Converting {rel}")
    subprocess.run([
        "jupyter", "nbconvert",
        "--to", "html",
        "--output-dir", str(outdir),
        "--no-input",          # hide raw input cells if you wish
        "--template", "html-template",
        # "--template", "lab",   # use modern JupyterLab look
        str(nb)
    ], check=True)

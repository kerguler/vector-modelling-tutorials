from nbviewer.app import NBViewer

app = NBViewer()

# Start with explicit options
app.initialize([
    "--port=8080",
    "--no-cache",
    "--localfiles=/srv/tutorials",
    "--base-url=/tutorials-viewer/",
])

# Monkey-patch the routing table to expose local files at "/"
for (pattern, handler, opts) in list(app.handlers):
    if pattern.pattern.startswith("/localfile"):
        app.handlers.remove((pattern, handler, opts))
        app.handlers.insert(0, (r"/(.*)", handler, opts))
        break

app.start()

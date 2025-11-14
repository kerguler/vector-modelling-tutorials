from nbviewer.app import NBViewer
from nbviewer.providers.local.provider import LocalHandler

app = NBViewer()
app.initialize([
    "--port=8080",
    "--no-cache",
    "--base-url=/tutorials-viewer/",
])

# Replace the default /localfile route with /
# (effectively hides 'localfile' in URLs)
app.handlers.insert(
    0,  # highest priority
    (r"/(.*)", LocalHandler, {"localfile_path": "/srv/tutorials"})
)

app.start()

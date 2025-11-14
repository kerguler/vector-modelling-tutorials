from nbviewer.app import NBViewer

app = NBViewer()

# Basic settings
app.initialize([
    '--port=8080',
    '--provider=local',
    '--localfiles=/srv/tutorials',
    '--base-url=/tutorials-viewer/',
])

# 👇 The newer nbviewer automatically mounts the local provider at /localfile.
# To move it to the root, patch the config before starting.
app.local_handlers = [
    (r"/(.*)", app.providers['local'].handler_class, dict(provider=app.providers['local']))
]

app.start()

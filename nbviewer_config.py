from nbviewer.app import NBViewer
from nbviewer.providers.local import LocalProvider

c = NBViewer.config
c.NBViewer.base_url = "/tutorials-viewer/"
c.NBViewer.provider = "local"
c.NBViewer.port = 8080
c.NBViewer.debug = True

# Local provider settings
c.LocalProvider.localfiles = "/srv/tutorials"
c.LocalProvider.urlpath = "/"

app = NBViewer()
app.initialize([])
app.start()

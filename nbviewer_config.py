c = get_config()

# Base URL under proxy
c.NBViewer.base_url = '/tutorials-viewer/'

# Provider setup
c.NBViewer.provider = 'local'
c.LocalProvider.localfiles = '/srv/tutorials'

# 👇 this replaces --localfiles-urlpath
c.LocalProvider.urlpath = '/'

# Port and debugging
c.NBViewer.port = 8080
c.NBViewer.debug = True

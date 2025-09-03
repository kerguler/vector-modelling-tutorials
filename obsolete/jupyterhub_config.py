c = get_config()  #noqa

c.JupyterHub.authenticator_class = 'jupyterhub.auth.DummyAuthenticator'
c.JupyterHub.hub_ip = 'jupyterhub'
c.JupyterHub.spawner_class = 'dockerspawner.DockerSpawner'

c.DummyAuthenticator.allow_all = True
c.DummyAuthenticator.allow_existing_users = True
c.DummyAuthenticator.password = 'Nicosia2025'

c.DockerSpawner.port = 8888
c.DockerSpawner.use_internal_ip = True
c.DockerSpawner.default_url = '/lab'
c.DockerSpawner.notebook_dir = '/code'
c.DockerSpawner.cmd = ['jupyterhub-singleuser']
c.DockerSpawner.debug = True
c.DockerSpawner.image = 'vector-modelling-tutorials'
c.DockerSpawner.network_name = 'jupyterhub-net'
c.DockerSpawner.remove = True

c.JupyterHub.hub_connect_ip = 'jupyterhub'
c.Spawner.args = ['--ServerApp.ip=0.0.0.0']

c.Spawner.start_timeout = 180
c.Spawner.http_timeout = 120
c.Spawner.default_url = '/lab'
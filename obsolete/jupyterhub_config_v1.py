# JupyterHub with DockerSpawner and DummyAuthenticator for local workshops
# File: jupyterhub_config.py

import os
from dockerspawner import DockerSpawner

# Use DummyAuthenticator (no real authentication — workshop/demo only!)
c.JupyterHub.authenticator_class = 'dummyauthenticator.DummyAuthenticator'
c.DummyAuthenticator.password = 'workshop2025'  # Shared password for all users
c.Authenticator.allow_all = True # For the new version

# Spawn a Docker container for each user
c.JupyterHub.spawner_class = 'dockerspawner.DockerSpawner'

# Image used for user containers
c.DockerSpawner.image = 'vector-modelling-tutorials'

# Notebook directory inside container
notebook_dir = '/home/jovyan/work'
c.DockerSpawner.notebook_dir = notebook_dir
c.DockerSpawner.volumes = { 'jupyterhub-user-{username}': notebook_dir }

# Network setup
c.DockerSpawner.use_internal_ip = True
c.DockerSpawner.network_name = 'jupyterhub-net'
c.DockerSpawner.extra_host_config = { 'network_mode': 'jupyterhub-net' }
c.DockerSpawner.extra_create_kwargs = {}
c.DockerSpawner.environment = {}
c.DockerSpawner.cmd = ["jupyterhub-singleuser"]

c.Spawner.debug = True

c.JupyterHub.hub_ip = 'jupyterhub'

# Tell DockerSpawner to use the remote Docker daemon from dind
# c.DockerSpawner.client_kwargs = {
#     'base_url': 'tcp://docker:2375'
# }

# Cleanup containers when they stop
c.DockerSpawner.remove = True

# Set default URL to JupyterLab
c.Spawner.default_url = '/lab'

# Port JupyterHub runs on
c.JupyterHub.port = 8000

import os
from dotenv import load_dotenv
load_dotenv(dotenv_path="/srv/jupyterhub/.env")  # mount .env into Hub container

HUB_CONNECT_URL = os.getenv("HUB_CONNECT_URL", "http://tutorials-jupyterhub:8000")
compose_name = os.getenv("COMPOSE_PROJECT_NAME", "")
shared_data_folder = os.getenv("SHARED_DATA_DIR", "")
idle_token = os.getenv("IDLE_CULLER_TOKEN")

# --- Core Hub ---
c.JupyterHub.bind_url = "http://:8000"
c.JupyterHub.cookie_secret_file = "/srv/jupyterhub/jupyterhub_cookie_secret"
c.JupyterHub.db_url = "sqlite:////srv/jupyterhub/jupyterhub.sqlite"
c.JupyterHub.log_level = "INFO"
c.JupyterHub.cleanup_servers = True

# Auth: NativeAuthenticator + temporary bypass so admin can log in
c.JupyterHub.authenticator_class = "nativeauthenticator.NativeAuthenticator"
c.NativeAuthenticator.open_signup = True        # show Sign Up
c.Authenticator.admin_users = {"admin"}         # the admin username
c.Authenticator.any_allow_config = True         # silence warning
c.Authenticator.allow_all = True               # <-- TEMPORARY, we will remove later
c.JupyterHub.log_level = "DEBUG"

# --- Spawner: Docker ---
c.JupyterHub.spawner_class = "dockerspawner.DockerSpawner"
c.DockerSpawner.image = "vector-modelling-tutorial-user:latest"
c.DockerSpawner.network_name = compose_name if compose_name else "tutorials-jhub" # f"{compose_name}_jhub" if compose_name else "jhub_jhub"
c.DockerSpawner.use_internal_ip = True
c.DockerSpawner.pull_policy = "IfNotPresent"

# Callback from single-user servers to Hub
c.DockerSpawner.hub_connect_url = HUB_CONNECT_URL

# Users land in JupyterLab
c.Spawner.default_url = "/lab"

# Per-user persistent home
c.DockerSpawner.volumes = {
    "jupyter-{username}": "/home/jovyan"
}

if shared_data_folder:
    c.DockerSpawner.volumes[shared_data_folder] = {
        "bind": "/srv/shared",
        "mode": "ro",
    }

# Basic resource limits (optional)
c.Spawner.mem_limit = "4G"
c.Spawner.cpu_limit = 2

# Clean up stopped user containers automatically
c.DockerSpawner.remove = True

# If you double-proxy, trust your inner proxy CIDR/IP (example shown)
# c.JupyterHub.trusted_downstream_ips = ["172.18.0.0/16"]

# --- Idle culler service ---
if not idle_token:
    raise SystemExit("IDLE_CULLER_TOKEN is not set; add it to your environment or .env file.")

c.JupyterHub.load_roles = [
    {
        "name": "culler",
        "description": "Allow idle-culler service to list users and cull servers",
        "scopes": ["list:users", "read:servers", "delete:servers"],
        "services": ["culler"],
    }
]

c.JupyterHub.services = [
    {
        "name": "culler",
        "command": [
            "jupyterhub-idle-culler",
            "--timeout=3600",
            "--cull-every=300",
            "--url=http://127.0.0.1:8000/hub/api",
        ],
        "api_token": idle_token,
    }
]

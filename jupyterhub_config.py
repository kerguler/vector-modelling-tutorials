import os
from dotenv import load_dotenv
load_dotenv(dotenv_path="/srv/jupyterhub/.env")  # mount .env into Hub container

HUB_CONNECT_URL = os.getenv("HUB_CONNECT_URL", "http://tutorials-jupyterhub:8000")
compose_name = os.getenv("COMPOSE_PROJECT_NAME", "")
shared_data_folder = os.getenv("SHARED_DATA_FOLDER", "")
shared_tutorials_folder = os.getenv("SHARED_TUTORIALS_FOLDER", "")
idle_token = os.getenv("IDLE_CULLER_TOKEN")

# --- Core Hub ---
c.JupyterHub.bind_url = "http://:8000"
c.JupyterHub.cookie_secret_file = "/srv/jupyterhub/jupyterhub_cookie_secret"
c.JupyterHub.db_url = "sqlite:////srv/jupyterhub/jupyterhub.sqlite"
c.JupyterHub.log_level = "INFO"
c.JupyterHub.cleanup_servers = True

# Auth: PAMAuthenticator
c.JupyterHub.authenticator_class = "jupyterhub.auth.PAMAuthenticator"
# PAM uses system accounts — users must exist on the host.
# To create users manually (Linux):
#   sudo useradd -m username && sudo passwd username
# Admin users (these must also exist in the host system)
c.Authenticator.admin_users = {"admin"}
c.Authenticator.allowed_users = {"admin"}
# PAM service name — "login" works for most systems,
# but could be "sshd", "lightdm", or custom, depending on /etc/pam.d/.
c.PAMAuthenticator.service = "login"
# Optional: automatically create the user’s home directory if missing
c.PAMAuthenticator.create_system_users = True
# Enable users to change their passwords
c.JupyterHub.services = [
    {
        'name': 'change-password',
        'command': ['jupyterhub-change-password-service'],
        'url': 'http://127.0.0.1:8888',
        'environment': {
            'CONFIGPROXY_AUTH_TOKEN': os.getenv('CONFIGPROXY_AUTH_TOKEN', ''),
        },
        'admin': True,
    }
]

# --- Spawner: Docker ---
c.JupyterHub.spawner_class = "dockerspawner.DockerSpawner"
c.DockerSpawner.image = "vector-modelling-tutorial-user:latest"
c.DockerSpawner.network_name = f"{compose_name}_jhub" if compose_name else "jhub_jhub"
c.DockerSpawner.use_internal_ip = True
c.DockerSpawner.pull_policy = "IfNotPresent"
c.DockerSpawner.hub_connect_url = HUB_CONNECT_URL
c.Spawner.default_url = "/lab"
c.DockerSpawner.name_template = "tutorials-{username}"

# Per-user persistent home
c.DockerSpawner.volumes = {
    "tutorials-{username}": "/home/jovyan"
}

if shared_data_folder:
    c.DockerSpawner.volumes[shared_data_folder] = {
        "bind": "/srv/shared",
        "mode": "ro",
    }

if shared_tutorials_folder:
    c.DockerSpawner.volumes[shared_tutorials_folder] = {
        "bind": "/srv/tutorials",
        "mode": "ro",
    }

# Basic resource limits (optional)
c.Spawner.mem_limit = "4G"
c.Spawner.cpu_limit = 2

# Requires xfs partition with pquota enabled
# c.DockerSpawner.extra_host_config = {
#     "storage_opt": {
#         "size": "5G"
#     }
# }

# allow more time for user containers to come online
c.Spawner.http_timeout = 120      # wait up to 120s for JupyterLab to reply
c.Spawner.start_timeout = 180     # allow 3 minutes total for startup

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

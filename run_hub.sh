#!/usr/bin/env bash

set -euo pipefail

# === Settings ===
ENV_FILE=".env"

# Pick docker compose command (v2 or v1)
if command -v docker >/dev/null 2>&1 && docker compose version >/dev/null 2>&1; then
  COMPOSE="docker compose"
elif command -v docker-compose >/dev/null 2>&1; then
  COMPOSE="docker-compose"
else
  echo "ERROR: Docker Compose not found. Install Docker Desktop / docker compose." >&2
  exit 1
fi

gen_token() {
  # Prefer Python for portability; fall back to openssl if available
  if command -v python >/dev/null 2>&1; then
    python -c "import secrets; print(secrets.token_hex(32))"
  elif command -v python3 >/dev/null 2>&1; then
    python3 -c "import secrets; print(secrets.token_hex(32))"
  elif command -v openssl >/dev/null 2>&1; then
    openssl rand -hex 32
  else
    # very last resort
    date +%s%N | sha256sum | awk '{print $1}'
  fi
}

ensure_env() {
  touch "$ENV_FILE"

  grep -q '^COMPOSE_PROJECT_NAME=' "$ENV_FILE" 2>/dev/null || \
    echo "COMPOSE_PROJECT_NAME=tutorials-jhub" >> "$ENV_FILE"
  
  grep -q '^HUB_CONNECT_URL=' "$ENV_FILE" 2>/dev/null || \
    echo "HUB_CONNECT_URL=http://tutorials-jupyterhub:8000" >> "$ENV_FILE"

  if ! grep -q '^IDLE_CULLER_TOKEN=' "$ENV_FILE" 2>/dev/null; then
    echo "Generating IDLE_CULLER_TOKEN into .env"
    echo "IDLE_CULLER_TOKEN=$(gen_token)" >> "$ENV_FILE"
  fi

  if ! grep -q '^SHARED_DATA_FOLDER=' "$ENV_FILE" 2>/dev/null; then
    # Default to ./data relative to the project root
    echo "Adding SHARED_DATA_FOLDER into .env"
    echo "SHARED_DATA_FOLDER=$(pwd)/data" >> "$ENV_FILE"
  fi
}

main() {
  echo "== Ensuring .env =="
  ensure_env

  echo "== Building singleuser image =="
  $COMPOSE build tutorials-singleuser

  echo "== Building jupyterhub image =="
  $COMPOSE build tutorials-jupyterhub
  echo "== Clearing stale Hub proxy PID (if any) =="
  $COMPOSE run --rm -u root tutorials-jupyterhub sh -lc 'rm -f /srv/jupyterhub/jupyterhub-proxy.pid' || true

  echo "== Starting stack =="
  $COMPOSE --env-file $ENV_FILE up -d 

  echo
  echo "✅ JupyterHub starting. Open: http://localhost"
  echo "   First time:"
  echo "     1) Sign Up with username 'admin' (password of your choice)"
  echo "     2) Log in as 'admin'"
  echo "     3) Approve other users at /hub/admin"
  echo
}

main "$@"

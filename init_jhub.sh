#!/usr/bin/env bash
set -e

DB_PATH="/srv/jupyterhub/jupyterhub.sqlite"

echo "[init_jhub] Starting initialization..."

# Ensure directory exists
mkdir -p "$(dirname "$DB_PATH")"

# If the DB exists, ensure native_authenticator_users table exists
if [ -f "$DB_PATH" ]; then
  echo "[init_jhub] Checking database integrity..."
  TABLE_COUNT=$(sqlite3 "$DB_PATH" "SELECT count(name) FROM sqlite_master WHERE type='table' AND name='native_authenticator_users';")
  if [ "$TABLE_COUNT" -eq 0 ]; then
    echo "[init_jhub] Creating missing table native_authenticator_users..."
    sqlite3 "$DB_PATH" <<'SQL'
CREATE TABLE IF NOT EXISTS native_authenticator_users (
    username TEXT PRIMARY KEY,
    password TEXT,
    is_admin INTEGER DEFAULT 0,
    is_approved INTEGER DEFAULT 0,
    last_activity INTEGER DEFAULT 0,
    failed_login_attempts INTEGER DEFAULT 0
);
SQL
  else
    echo "[init_jhub] Table native_authenticator_users already exists."
  fi
else
  echo "[init_jhub] Database not found. It will be initialized by JupyterHub."
fi

# Run JupyterHub core migrations (safe to run multiple times)
echo "[init_jhub] Running jupyterhub upgrade-db..."
jupyterhub upgrade-db || echo "[init_jhub] Warning: upgrade-db failed, continuing."

echo "[init_jhub] Launching JupyterHub..."
exec jupyterhub -f /srv/jupyterhub/jupyterhub_config.py

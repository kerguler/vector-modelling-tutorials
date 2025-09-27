#!/bin/bash
set -e

if [ ! -d /home/jovyan/tutorials ]; then
  cp -r /srv/tutorials /home/jovyan/tutorials
fi

# ensure jovyan owns its full home (covers .local too)
chown -R jovyan:users /home/jovyan

chmod +x /home/jovyan/tutorials/mina/shiny-wrapper.R
cat >> /home/jovyan/.jupyter/jupyter_server_config.py <<'EOF'
c.ServerProxy.servers = {
    'shiny': {
        'command': ['Rscript', '/home/jovyan/tutorials/mina/shiny-wrapper.R'],
        'timeout': 3000,
        'launcher_entry': {
            'title': 'Shiny App',
            'icon_path': ''  # you can give a PNG/SVG path if you like
        }
    }
}
EOF

exec start.sh "$@"

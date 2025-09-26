#!/bin/bash
set -e

# ensure jovyan owns its full home (covers .local too)
chown -R jovyan:users /home/jovyan

if [ ! -d /home/jovyan/tutorials ]; then
  cp -r /srv/tutorials /home/jovyan/tutorials
fi

exec start.sh "$@"

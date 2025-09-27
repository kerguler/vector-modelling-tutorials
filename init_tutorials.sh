#!/bin/bash
set -e

if [ ! -d /home/jovyan/tutorials ]; then
  cp -r /srv/tutorials /home/jovyan/tutorials
fi

# ensure jovyan owns its full home (covers .local too)
chown -R jovyan:users /home/jovyan

exec start.sh "$@"

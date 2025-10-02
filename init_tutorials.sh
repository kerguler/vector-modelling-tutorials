#!/bin/bash
set -e

if [ ! -d /home/jovyan/tutorials ]; then
  ln -s /srv/tutorials /home/jovyan/tutorials
fi

chown -R jovyan:users /home/jovyan

exec start.sh "$@"
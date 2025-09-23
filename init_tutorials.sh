#!/bin/bash
set -e

if [ ! -d /home/jovyan/tutorials ]; then
  cp -r /srv/tutorials /home/jovyan/tutorials
  chown -R jovyan:users /home/jovyan/tutorials
fi

exec start.sh "$@"

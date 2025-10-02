#!/bin/bash
set -e

chown -R $NB_UID:$NB_GID /opt/conda

if [ ! -d /home/jovyan/tutorials ]; then
  ln -s /srv/tutorials $HOME/tutorials
fi

chown -R $NB_UID:$NB_GID $HOME

exec start.sh "$@"
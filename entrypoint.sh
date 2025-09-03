#!/bin/bash
set -e

# Load conda
source /opt/conda/etc/profile.d/conda.sh
conda activate base

# Run Jupyter in the foreground (better for Docker)
exec jupyter lab \
    --allow-root \
    --no-browser \
    --ip=0.0.0.0 \
    --port=8888 \
    --NotebookApp.token='' \
    --NotebookApp.password='' \
    --NotebookApp.allow_origin='*'
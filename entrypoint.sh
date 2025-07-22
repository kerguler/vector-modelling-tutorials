#!/bin/bash

source /opt/conda/etc/profile.d/conda.sh
conda activate base

# Start JupyterLab
echo "Starting JupyterLab..."
jupyter lab --allow-root --no-browser --ip=0.0.0.0 --port=8888 --NotebookApp.token='' --NotebookApp.password='' --NotebookApp.allow_origin='*' &

# Access at http://localhost:8888/

# Wait for all background jobs
wait

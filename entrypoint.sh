#!/bin/bash
set -e

# Load conda
source /opt/conda/etc/profile.d/conda.sh
conda activate base

#!/bin/bash
set -e

# Number of users (default: 1 if not provided)
N=${N_USERS:-1}
BASE_PORT=8000

for i in $(seq 1 $N); do
    USER="user$(printf "%02d" $i)"
    PORT=$((BASE_PORT + i - 1))

    # Create user if not exists
    if ! id -u $USER >/dev/null 2>&1; then
        useradd -m -s /bin/bash $USER
        echo "$USER:$USER" | chpasswd
    fi

    # Ensure work directory
    mkdir -p /home/$USER/notebooks
    chown -R $USER:$USER /home/$USER
    cp -r /code/tutorials /home/$USER/notebooks

    # Launch JupyterLab as the user
    # Use --ip=0.0.0.0 to allow external connections
    # Run in background with nohup
    su $USER -c "nohup jupyter lab \
        --ip=0.0.0.0 \
        --port=${PORT} \
        --no-browser \
        --NotebookApp.token='' \
        --NotebookApp.password='' \
        --notebook-dir=/home/${USER}/notebooks \
        > /home/${USER}/jupyter.log 2>&1 &"
    
    echo "Started JupyterLab for $USER on port $PORT"
done

# Keep container alive
tail -f /dev/null
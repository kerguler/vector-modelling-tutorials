#!/bin/bash

echo "Installing and setting up the environment..."

if [ "$(uname -m)" = "x86_64" ]; then \
      FILE=Miniconda3-py311_24.7.1-0-Linux-x86_64.sh; \
    elif [ "$(uname -m)" = "aarch64" ]; then \
      FILE=Miniconda3-py311_24.7.1-0-Linux-aarch64.sh; \
    else \
      echo "Unsupported architecture $(uname -m)" && exit 1; \
    fi

if [ -f "src/$FILE" ]; then \
      echo "Using local copy of $FILE"; \
      cp "src/$FILE" /tmp/miniconda.sh; \
    else \
      echo "Downloading $FILE from Anaconda"; \
      wget --quiet "https://repo.anaconda.com/miniconda/$FILE" -O /tmp/miniconda.sh; \
    fi

/bin/bash /tmp/miniconda.sh -b -p /opt/conda && \
       rm /tmp/miniconda.sh

# Set up the Anaconda path
export PATH=/opt/conda/bin:$PATH

# Run conda
conda init

# Update conda and install packages
conda update -n base -c defaults conda && \
    conda config --add channels defaults && \
    conda install -c conda-forge -y \
        pandas \
        xarray \
        netCDF4 \
        mpi4py \
        nodejs \
        jupyterlab \
        r-codetools \
        r-IRkernel

conda activate base

R -e "IRkernel::installspec(user = FALSE)"

# Copy the Python population package and install it
chmod +x /code/src/population/run.sh
/code/src/population/run.sh
#!/bin/bash

echo "Installing and setting up the environment..."

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
        jupyter \
        jupyterlab \
        r-codetools \
        r-IRkernel

conda activate base

R -e "IRkernel::installspec(user = FALSE)"

# Copy the Python population package and install it
chmod +x /code/src/population/run.sh
/code/src/population/run.sh

cd /code/tutorials/kamil
npm install popjson
ln -s node_modules/popjson/wrappers/population.R ./
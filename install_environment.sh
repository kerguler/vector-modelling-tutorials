#!/bin/bash

echo "Installing and setting up the environment..."

# Set up the Anaconda path
export PATH=/opt/conda/bin:$PATH

# Run conda
conda init
conda activate base

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

# For arbocartoR
git clone https://gitlab.cirad.fr/astre/arbocartoR.git ./src/arbocartoR
conda install -c conda-forge -y \
  r-remotes
R -e "remotes::install_local('./src/arbocartoR', upgrade = 'always')"

# Activate kernel for Jupyter Lab
R -e "IRkernel::installspec(user = FALSE)"

# Copy the Python population package and install it
chmod +x /code/src/population/run.sh
/code/src/population/run.sh

cd /code/tutorials/kamil
npm install popjson
ln -s node_modules/popjson/wrappers/population.R ./
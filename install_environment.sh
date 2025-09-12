#!/bin/bash

echo "Installing and setting up the environment..."

# Set up the Anaconda path
export PATH=/opt/conda/bin:$PATH

# Update conda and install packages
conda update -n base -c defaults conda && \
    conda config --add channels defaults && \
    conda install -n base -c conda-forge -y \
        pandas \
        xarray \
        matplotlib \
        netCDF4 \
        mpi4py \
        nodejs \
        gdal proj geos \
        jupyter \
        jupyterlab \
        openjdk \
        cdsapi \
        r-codetools \
        r-recommended \
        r-remotes \
        r-tidyterra r-sf r-fs r-sass r-stringi r-stringr r-tidyr r-reshape2 r-recipes r-units r-s2 r-bslib r-caret r-rmarkdown r-Ecume r-sass r-bslib r-cachem r-httpuv r-htmlwidgets r-shiny r-dygraphs r-SimInf \
        r-devtools r-drc \
        r-IRkernel

R -e 'install.packages("TDLM", repos="https://cran.rstudio.com")'
R -e 'install.packages("data.table", repos="https://cran.rstudio.com")'
R -e 'install.packages("SimInf", repos="https://cran.rstudio.com", configure.args="--host=host")'
R -e 'install.packages("sf", repos="https://cran.rstudio.com", configure.args="--host=host")'
R -e 'install.packages("htmltools", repos="https://cran.rstudio.com")'
R -e 'install.packages("foreach",repos="https://cran.rstudio.com")'
R -e 'install.packages("ggplot2",repos="https://cran.rstudio.com")'

# For arbocartoR
git clone https://gitlab.cirad.fr/astre/arbocartoR.git ./src/arbocartoR
R -e "remotes::install_local('./src/arbocartoR', lib = .libPaths()[1], upgrade = 'never')"

# For dynamAedes
git clone --branch development https://github.com/mattmar/dynamAedes.git ./src/dynamAedes
R -e "devtools::install_local('./src/dynamAedes', lib = .libPaths()[1], upgrade = 'never')"

# Activate R kernel for Jupyter Lab
R -e "IRkernel::installspec(user = FALSE, prefix = '/opt/conda')"

# Copy the Python population package and install it
chmod +x /code/src/population/run.sh
/code/src/population/run.sh

cd /code/tutorials/kamil
npm install popjson
ln -s node_modules/popjson/wrappers/population.R ./

# --- Step 1: Set up the Julia environment
conda create -n julia_env -y python=3.11
conda activate julia_env

# --- Step 2: Install Julia
conda install -n base -c conda-forge -y jupyterlab julia

# --- Step 3: Fix CA certificate issue for Julia (if needed) ---
# Find Conda's CA bundle (used by Julia's HTTPS downloads)
CA_CERT=$(find $CONDA_PREFIX -name "cacert.pem" | head -n 1)
export JULIA_SSL_CA_ROOTS_PATH="$CA_CERT"

# --- Step 4: Install IJulia and register the kernel in the conda env path ---
julia -e '
    using Pkg;
    Pkg.add("IJulia");
    using IJulia;
    installkernel("Julia", "--project=@.", "--prefix", joinpath(ENV["CONDA_PREFIX"], "share", "jupyter"))
'

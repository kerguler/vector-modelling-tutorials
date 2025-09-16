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
        cartopy \
        r-codetools \
        r-recommended \
        r-remotes \
        r-tidyterra r-sf r-fs r-sass r-stringi r-stringr r-tidyr r-reshape2 r-recipes r-units r-s2 r-bslib r-caret r-rmarkdown r-Ecume r-sass r-bslib r-cachem r-httpuv r-htmlwidgets r-shiny r-dygraphs r-SimInf \
        r-devtools r-drc \
        r-doParallel r-geosphere \
        r-PBSddesolve r-wesanderson \
        r-deSolve r-shiny r-torch \
        r-nimble r-HDInterval r-IDPmisc r-truncnorm r-httr \
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
R -e "remotes::install_local('./src/arbocartoR', lib = .libPaths()[1], dependencies=FALSE, upgrade = 'never')"

# For dynamAedes
git clone --branch development https://github.com/mattmar/dynamAedes.git ./src/dynamAedes
R -e "devtools::install_local('./src/dynamAedes', lib = .libPaths()[1], dependencies=FALSE, upgrade = 'never')"

# For bayesTPC
git clone https://github.com/johnwilliamsmithjr/bayesTPC.git ./src/bayesTPC
R -e "devtools::install_local('./src/bayesTPC', lib = .libPaths()[1], dependencies=FALSE, upgrade = 'never')"

# For torch
TORCH_DIR="/srv/torch"
mkdir -p "$TORCH_DIR"
chmod 777 "$TORCH_DIR"
R -e "install.packages('torch', repos = 'https://cloud.r-project.org')"
R --vanilla -e "Sys.setenv(TORCH_HOME='$TORCH_DIR'); library(torch); torch::install_torch()"

# Activate R kernel for Jupyter Lab
R -e "IRkernel::installspec(user = FALSE, prefix = '/opt/conda')"

# Copy the Python population package and install it
chmod +x /code/src/population/run.sh
/code/src/population/run.sh

cd /code/tutorials/kamil
npm install popjson
ln -s node_modules/popjson/wrappers/population.R ./

# --- Julia in a Conda env (safer invocation) ---
# --- Official Julia (recommended) ---
export JULIA_VERSION=1.10.5
curl -fsSL https://julialang-s3.julialang.org/bin/linux/x64/${JULIA_VERSION%.*}/julia-${JULIA_VERSION}-linux-x86_64.tar.gz \
  | tar -xz -C /opt
ln -sf /opt/julia-${JULIA_VERSION}/bin/julia /usr/local/bin/julia

# Shared depot
export JULIA_DEPOT_PATH=/opt/julia_depot
mkdir -p /opt/julia_depot
chmod -R a+rwx /opt/julia_depot

# Make it permanent for all users
echo 'export JULIA_DEPOT_PATH=/opt/julia_depot:$HOME/.julia' > /etc/profile.d/julia.sh

# Clean cert vars
unset SSL_CERT_FILE
unset CURL_CA_BUNDLE
export JULIA_PKG_SERVER=""
export JULIA_PKG_USE_CLI_GIT="true"

# Install system-wide packages into the shared depot
julia -e 'using Pkg; Pkg.add([
    "IJulia", "Dates", "DifferentialEquations", "Dierckx",
    "Plots", "CSV", "Interpolations", "QuadGK",
    "Statistics", "DataFrames", "NCDatasets", "MPI"
]); Pkg.precompile()'

# Install the Jupyter kernel globally
julia -e 'using IJulia; installkernel("Julia", env=Dict("JULIA_DEPOT_PATH"=>"/opt/julia_depot"))'
mv $HOME/.local/share/jupyter/kernels/julia* /opt/conda/share/jupyter/kernels/

# System-wide history location per user
echo "export JULIA_HISTORY=\$HOME/.julia/logs/repl_history.jl" > /etc/profile.d/julia.sh
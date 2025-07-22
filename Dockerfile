# Use a recent stable version of Ubuntu as a parent image
FROM ubuntu:22.04

# Install required packages for the application
RUN apt-get update && apt-get install -y \
    software-properties-common \
    build-essential \
    wget \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    proj-bin \
    gnupg \
    libgdal-dev \
    libgsl-dev \
    gcc \
    libc6-dev \
    make \
    mpich \
    nco \
    supervisor \
    && apt-get clean

# Set the working directory in the Docker image
COPY ./ /code/
WORKDIR /code

# Download and install the latest Miniconda installer
RUN chmod +x /code/install_environment.sh
RUN /code/install_environment.sh

# Copy the Python population package and install it
RUN chmod +x /code/src/population/run.sh
RUN /code/src/population/run.sh

# Create a user for Jupyter
ENV NB_USER=vector-modeller
ENV NB_UID=1000
ENV NB_GID=1000

# Setup Supervisor to manage both services
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 8888

ENTRYPOINT ["/entrypoint.sh"]
# CMD ["bash"]
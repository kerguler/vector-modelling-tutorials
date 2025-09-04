# Use a recent stable version of Ubuntu as a parent image
# FROM ubuntu:22.04
FROM continuumio/miniconda3

# Install required packages for the application
RUN apt-get update && apt-get install -y \
    software-properties-common \
    build-essential \
    wget \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    proj-bin \
    libgdal-dev \
    gnupg \
    libgsl-dev \
    gcc \
    libc6-dev \
    make \
    mpich \
    supervisor \
    nco \
    && apt-get clean

# Set the working directory in the Docker image
COPY ./ /code/
WORKDIR /code

# Download and install the latest Miniconda installer
RUN chmod +x /code/install_environment.sh
RUN /code/install_environment.sh

# Document up to 100 possible ports (adjust to your needs)
EXPOSE 8000-8099

# Setup Supervisor to manage both services
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Default environment variable: number of users
ENV N_USERS=1

ENTRYPOINT ["/entrypoint.sh"]
# CMD ["bash"]
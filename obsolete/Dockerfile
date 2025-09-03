# Use a recent stable version of Ubuntu as a parent image
# FROM ubuntu:22.04
FROM jupyterhub/jupyterhub:latest

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

RUN pip install --no-cache-dir "jupyterlab>=4" jupyter_server dockerspawner jupyterhub-dummyauthenticator

# Set the working directory in the Docker image
COPY ./ /code/
WORKDIR /code

# Download and install the latest Miniconda installer
#RUN chmod +x /code/install_environment.sh
#RUN /code/install_environment.sh

# Create a user for Jupyter
ARG USERS="vector-modeller veclim"

RUN for user in $USERS; do \
      useradd -m -s /bin/bash $user && \
      cp -R /code/tutorials /home/$user && \
      chown -R $user:$user /home/$user ; \
    done

# Setup Supervisor to manage both services
# COPY entrypoint.sh /entrypoint.sh
# RUN chmod +x /entrypoint.sh

EXPOSE 8888

# ENTRYPOINT ["/entrypoint.sh"]
# CMD ["bash"]
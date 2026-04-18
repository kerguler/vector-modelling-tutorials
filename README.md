# Installation Instructions for VEClim Tutorials
## JupyterHub - JupyterLab Setup

This guide describes how to set up the VEClim tutorial environment using JupyterHub with JupyterLab, either locally or on a server.

### 🐳 Base Docker Image
The single-user Jupyter containers are built on the Docker image:
```
kerguler/vector-modelling-tutorial-base:latest
```
This image includes all required dependencies for running the VEClim tutorials.

By default, the setup will automatically pull this image from Docker Hub. Alternatively, you can build the image locally using the provided `Dockerfile.base`.

#### Build the image locally
```bash
docker build -f Dockerfile.base -t kerguler/vector-modelling-tutorial-base:latest .
```

#### (Optional) Push to Docker Hub
```bash
docker push kerguler/vector-modelling-tutorial-base:latest
```
💡 The `nohup` variants may be useful for long-running builds on remote servers.

### 🚀 Running JupyterHub
To launch the VEClim tutorial environment:
```bash
docker-compose up -d --build
```
This will:
- build (if necessary) and start all required containers
- launch a JupyterHub instance
- provide access to JupyterLab environments for users

Once running, you can access the interface via your browser (typically at http://localhost:8000 or the configured host).

## 🧑‍💻 Support
This software is provided **as is**, without any warranty.

If you encounter issues or have questions:
- open an issue in this repository
- or contact the [VEClim modelling team](mailto:veclim.cyi@gmail.com)
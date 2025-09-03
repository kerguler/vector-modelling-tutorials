# vector-modelling-tutorials

Tutorials for the **Vector Modelling Workshop**  
📍 *Nicosia, Cyprus — September 2025*

This repository contains interactive tutorials on climate-sensitive vector dynamics, epidemiological modelling, and related computational methods, designed for participants of the Climate-Sensitive Vector Dynamics Modelling Workshop, to be held in Nicosia, Cyprus, on 17–19 September 2025.

---

## 🚀 Getting Started

### Run with `setup.sh`

The repository provides a script `setup.sh` that manages JupyterLab containers for workshop participants.  

#### Start containers
```bash
./setup.sh <number_of_users>
```

This will:

- Build (if already built, reuses) the Docker image `vector-modelling-tutorials:latest`
- Launch `<number_of_users>` containers
- Map them to consecutive host ports starting at `8800`  
  (e.g., user1 → `http://localhost:8800`, user2 → `http://localhost:8801`, …)
- Assign each container a unique Jupyter token tied to its port  
  (e.g., `vector_modeller_8800` for port 8800)

#### Force rebuild
```bash
./setup.sh <number_of_users> --build
```
Adds a `--build` flag to rebuild the image before starting containers.

#### Stop containers
```bash
./setup.sh <number_of_users> --down
```
Stops and removes the specified number of containers.

---

### Option 2: Pull from Docker Hub

If you prefer not to build the image locally, you can run a single container (once published):

```bash
docker run -p 8888:8888 vector-modelling-tutorials
```

Then open:  
🔗 [http://localhost:8888](http://localhost:8888)

---

## 📦 What's Inside

The tutorials cover topics including:

- Structured population modelling of disease vectors
- Environmental drivers of vector-borne transmission
- Climate data access and bias correction
- Reproducible workflows using R, Python, and Jupyter

The image includes:

- R and RStudio Server
- JupyterLab with IRKernel (R) and Python 3
- Commonly used libraries for climate and epidemiological modelling

---

## 🛠 Requirements

- Docker (v20+ recommended)
- Linux/macOS terminal with execution permissions

---

## 🧑‍💻 Support

If you encounter issues or have questions during the workshop, please contact the organizing team or open an issue in this repository.

---

## 📅 About the Workshop

The **2nd Climate-Sensitive Vector Dynamics Modelling Workshop** brings together researchers, modelers, and practitioners to develop tools and exchange knowledge on climate-driven vector-borne disease risk.

For more info: [vectormodelling.com/Nicosia2025](https://vectormodelling.com/Nicosia2025)

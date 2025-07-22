# vector-modelling-tutorials

Tutorials for the **Vector Modelling Workshop**  
📍 *Nicosia, Cyprus — September 2025*

This repository contains interactive tutorials on climate-sensitive vector dynamics, epidemiological modelling, and related computational methods, designed for participants of the Climate-Sensitive Vector Dynamics Modelling Workshop, to be held in Nicosia, Cyprus, on 17-19 September 2025.

---

## 🚀 Getting Started

### Option 1: Build and Run Locally (Linux/macOS)

You can build the Docker image and launch JupyterLab using the provided setup script:

```bash
./setup.sh
```

This will:

- Build the Docker image
- Start a container running JupyterLab
- Map it to your local port 8888

Access the interface at:  
🔗 [http://localhost:8888](http://localhost:8888)

---

### Option 2: Pull from Docker Hub

If you prefer not to build the image yourself, you can download it from Docker Hub (once available):

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

# Aedes-borne diseases risk assessment

<!-- badges: start -->
<!-- badges: end -->

The goal of arbocartoR is to guide the local development of effective strategies and interventions to mitigate the impact of these diseases on global health.

## Installation

You can install the development version of arbocartoR with the following code.
Note that on Windows, you need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

``` r
install.packages("remotes")
library(remotes)
remotes::install_gitlab("astre/arbocartoR", host = "https://gitlab.cirad.fr")
```

## Example

This is a basic example which shows you how to run simulations:

``` r
library(arbocartoR)
library(magrittr)

data(parcels)
data(meteo)

parcels %<>% .[startsWith(ID, "06"),]

traj <- run_arbocartoR( parcels = parcels,
                        vector = "Ae. albopictus (D)",
                        virus = "DEN",
                        meteo = meteo)
                        
traj[[1]]
```



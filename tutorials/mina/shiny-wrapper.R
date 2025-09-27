#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
})

# Read port/host from jupyter-server-proxy
port <- as.numeric(Sys.getenv("PORT", "8050"))
host <- Sys.getenv("HOST", "127.0.0.1")

# Load your app definition (must define ui and server)
source("/home/jovyan/tutorials/mina/pinn-shinyv4V5.R")

message(sprintf("Starting Shiny app from pinn-shinyv4V5.R on %s:%d", host, port))

shiny::shinyApp(
  ui = ui,
  server = server,
  options = list(host = host, port = port, launch.browser = FALSE)
)

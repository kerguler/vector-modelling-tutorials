#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
})

filename <- "pinn-shinyv4V5.R"

# Read port/host from jupyter-server-proxy
port <- as.numeric(Sys.getenv("PORT", "8050"))
host <- Sys.getenv("HOST", "127.0.0.1")

# Load your app definition (must define ui and server)
source(sprintf("./%s", filename))

message(sprintf("Starting Shiny app from %s on %s:%d", filename, host, port))

shiny::shinyApp(
  ui = ui,
  server = server,
  options = list(host = host, port = port, launch.browser = FALSE)
)

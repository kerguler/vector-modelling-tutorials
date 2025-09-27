# app.R
# PINN vs plain NN on sparse seasonal data (CPU-friendly, seems stable) # 16/09/2025
# bugs: server gg; solved (kamil docker); mp: added spinner L13 (shinyxssloaders); L209-214

# -------- deps --------------

require_pkgs <- function(pkgs){
  new <- pkgs[!(pkgs %in% installed.packages()[,1])]
  if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, require, character.only = TRUE))
}

require_pkgs(c("shiny","ggplot2","dplyr","tidyr","deSolve", "shinycssloaders"))

if (R.version$arch == 'aarch64') {
  library(reticulate)
  use_condaenv('base', required = TRUE)
  torch <- import('torch')

  make_torch_wrapper <- function(name) {
    f <- torch[[name]]
    assign(
      paste0("torch_", name),
      function(...) f(...),
      envir = .GlobalEnv
    )
  }

  # Wrap some common ops
  methods <- c("tensor", "sin", "cos", "exp", "log", "add", "mul", "matmul", "float")
  lapply(methods, make_torch_wrapper)

  # R-wrapper for pytorch
  # -------- reticulate -> torch --------------------
  nn_linear <- function(in_features, out_features) {
    torch$nn$Linear(as.integer(in_features), as.integer(out_features))
  }
  nn_relu <- function() {
    torch$nn$ReLU()
  }
  torch_tensor <- function(x, dtype = torch$float32) {
    torch$tensor(x, dtype = dtype)
  }
  nn_module <- function(name, initialize, forward) {
    # Create a new subclass of nn.Module
    torch$nn$Module[['__class__']](
      `__init__` = initialize,
      forward = forward
    )
  }
} else {
  if (!requireNamespace("torch", quietly = TRUE)) install.packages("torch", repos = "https://cloud.r-project.org")

  library(torch)

  if (!torch_is_installed()) {
    torch::install_torch()
    showModal(modalDialog(
      title = "Setup complete",
      "Torch was just installed. Please restart the app (Session -> Restart R, then run again).",
      easyClose = TRUE
    ))
    return(invisible(NULL))
  }
}

# -------- ground truth: simple seasonal linear ODE --------
# dy/dt = -alpha*y + beta*sin(omega*t) ; bounded, seasonal
simulate_truth <- function(alpha=0.2, beta=1.0, period=180, t_max=200, dt=1, y0=0){
  omega <- 2*pi/period
  times <- seq(0, t_max, by = dt)
  rhs <- function(t, y, p){
    with(as.list(p), {
      list( -alpha*y + beta*sin(omega*t) )
    })
  }
  pars <- list(alpha=alpha, beta=beta, omega=omega)
  sol <- ode(y = c(y=y0), times = times, func = rhs, parms = pars, method = "rk4")
  df <- as.data.frame(sol)
  names(df) <- c("t","y")
  df
}

# -------- data generator: sparse noisy obs --------
make_data <- function(n_obs = 20, noise_sd = 0.2, seed = 1,
                      alpha=0.2, beta=1.0, period=180, t_max=200){
  set.seed(seed)
  truth <- simulate_truth(alpha=alpha, beta=beta, period=period, t_max=t_max)
  # pick observation times (stratified)
  t_all <- truth$t
  idx <- unique(as.integer(round(seq(1, length(t_all), length.out = n_obs))))
  # if duplicates collapsed, top up deterministically
  if(length(idx) < n_obs){
    missing <- setdiff(seq_len(length(t_all)), idx)
    idx <- sort(c(idx, head(missing, n_obs - length(idx))))
  }
  
  t_obs <- t_all[idx]
  y_true_at_obs <- truth$y[idx]
  y_obs <- y_true_at_obs + rnorm(length(idx), 0, noise_sd)
  list(truth=truth,
       obs=data.frame(t=t_obs, y=y_obs),
       period=period)
}

# -------- feature map (keeps autograd path from t) --------
# Harmonic features up to H (default 2). Autograd handles sin/cos chain rule.
feature_map <- function(t, period, H=2){
  omega <- 2*pi/period
  # Detach normalization stats to avoid polluting dy/dt
  mu  <- t$mean()$detach()
  sig <- (t$std(unbiased = FALSE) + 1e-8)$detach()
  t_norm <- (t - mu) / sig
  
  feats <- list(t_norm)
  for(h in 1:H){
    feats <- c(feats, list(torch_sin(h*omega*t), torch_cos(h*omega*t)))
  }
  torch_cat(feats, dim = 2)
}

# -------- small MLP (shared by NN & PINN) --------
mlp <- nn_module(
  initialize = function(in_dim, hidden=64, out_dim=1){
    self$l1 <- nn_linear(in_dim, hidden)
    self$l2 <- nn_linear(hidden, hidden)
    self$l3 <- nn_linear(hidden, out_dim)
  },
  forward = function(x){
    h <- torch_tanh(self$l1(x))
    h <- torch_tanh(self$l2(h))
    self$l3(h)  # linear output (real-valued)
  }
)

# -------- NN training (data-only) --------
train_nn <- function(df_truth, df_obs, period,
                     epochs=500, lr=1e-3, H=2, seed=1){
  torch_manual_seed(seed)
  t_full <- torch_tensor(matrix(df_truth$t, ncol=1), dtype=torch_float())
  t_obs  <- torch_tensor(matrix(df_obs$t,   ncol=1), dtype=torch_float())
  y_obs  <- torch_tensor(matrix(df_obs$y,   ncol=1), dtype=torch_float())
  
  net <- mlp(in_dim = 1 + 2*H, hidden=64, out_dim=1)
  opt <- optim_adam(net$parameters, lr = lr)
  
  hist <- data.frame(epoch=integer(), loss=double())
  
  for(e in 1:epochs){
    opt$zero_grad()
    X_obs <- feature_map(t_obs, period, H)
    y_hat <- net(X_obs)
    loss  <- torch_mean((y_hat - y_obs)$pow(2))
    loss$backward(); opt$step()
    if (e %% 20 == 0 || e == epochs)
      hist <- rbind(hist, data.frame(epoch=e, loss=as.numeric(loss$item())))
  }
  
  with_no_grad({
    X_full <- feature_map(t_full, period, H)
    y_full <- as.numeric(net(X_full)$squeeze())
  })
  list(pred = data.frame(t=df_truth$t, y=y_full), hist=hist, net=net)
}

# -------- PINN training (data + physics) --------
# Physics residual: r = dy_dt + alpha*y - beta*sin(omega t)
train_pinn <- function(df_truth, df_obs, period,
                       alpha=0.2, beta=1.0, lambda_phys=1.0,
                       epochs=500, lr=1e-3, H=2, seed=1){
  torch_manual_seed(seed)
  t_full <- torch_tensor(matrix(df_truth$t, ncol=1), dtype=torch_float())
  t_obs  <- torch_tensor(matrix(df_obs$t,   ncol=1), dtype=torch_float())
  y_obs  <- torch_tensor(matrix(df_obs$y,   ncol=1), dtype=torch_float())
  
  net <- mlp(in_dim = 1 + 2*H, hidden=64, out_dim=1)
  opt <- optim_adam(net$parameters, lr = lr)
  
  omega <- 2*pi/period
  hist <- data.frame(epoch=integer(), loss=double(), l_data=double(), l_phys=double())
  
  for(e in 1:epochs){
    opt$zero_grad()
    
    # data term
    X_obs <- feature_map(t_obs, period, H)
    y_hat_obs <- net(X_obs)
    l_data <- torch_mean((y_hat_obs - y_obs)$pow(2))
    
    # physics term on dense collocation (full grid)
    t_full$requires_grad_(TRUE)
    X_full <- feature_map(t_full, period, H)
    y_hat_full <- net(X_full)                # [N,1]
    ones <- torch_ones_like(y_hat_full)
    dy_dt <- autograd_grad(outputs = y_hat_full, inputs = t_full,
                           grad_outputs = ones, create_graph = TRUE)[[1]]
    rhs <- -alpha * y_hat_full + beta * torch_sin(omega * t_full)
    r <- dy_dt - rhs
    # light normalization of residual by scale of y and dy/dt
    scale <- (y_hat_full$abs()$mean() + dy_dt$abs()$mean() + 1e-3)
    l_phys <- torch_mean((r/scale)$pow(2))
    
    loss <- l_data + lambda_phys * l_phys
    loss$backward(); opt$step()
    
    if (e %% 20 == 0 || e == epochs)
      hist <- rbind(hist, data.frame(
        epoch=e,
        loss=as.numeric(loss$item()),
        l_data=as.numeric(l_data$item()),
        l_phys=as.numeric(l_phys$item())
      ))
  }
  
  with_no_grad({
    X_full <- feature_map(t_full, period, H)
    y_full <- as.numeric(net(X_full)$squeeze())
  })
  list(pred = data.frame(t=df_truth$t, y=y_full), hist=hist, net=net)
}

# -------- UI --------
ui <- fluidPage(
  titlePanel("NN vs PINN — Sparse Seasonal Signal (R/torch)"),
  sidebarLayout(
    sidebarPanel(
      h4("Signal & data"),
      sliderInput("period", "Seasonal period (days)", min=60, max=365, value=180, step=5),
      sliderInput("alpha",  "Damping α (1/day)", min=0.05, max=0.6, value=0.2, step=0.01),
      sliderInput("beta",   "Forcing β", min=0.2, max=2.0, value=1.0, step=0.1),
      sliderInput("nobs",   "Number of observations", min=8, max=60, value=20, step=1),
      sliderInput("noise",  "Obs noise SD", min=0.0, max=3.6, value=2.2, step=0.02),
      numericInput("seed",  "Seed", value = 1, min=1),
      
      h4("Training"),
      sliderInput("epochs", "Epochs", min=100, max=2000, value=600, step=50),
      numericInput("lr", "Learning rate", value = 1e-3, min=1e-5, step=1e-3),
      sliderInput("H", "Harmonic order H (features)", min=1, max=4, value=2, step=1),
      sliderInput("lambda", "PINN physics weight λ", min=0.0, max=10, value=1.0, step=0.2),
      
      actionButton("train", "Generate data & train", class="btn-primary"),
      helpText("Tip: increase λ to enforce physics more strongly, or increase epochs.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Fit",
          shinycssloaders::withSpinner(
            plotOutput("plot_fit", height = 360),
            type = 4, color = "#666666"
          )
        ),
        tabPanel("About",
                 tags$p("Goal: Compare a plain neural net (NN, data-only) versus a physics-informed neural net (PINN)."),
                 tags$p("True process follows dy/dt = -α y + β sin(ω t) with seasonal period."),
                 tags$ul(
                   tags$li("NN minimizes MSE on sparse noisy observations."),
                   tags$li("PINN minimizes MSE on observations + λ·MSE(ODE residual) on a dense time grid.")
                 )
        )
      )
    )
  )
)

# -------- server --------
server <- function(input, output, session){
  rv <- reactiveValues(df_truth=NULL, df_obs=NULL, nn=NULL, pinn=NULL)
  
  observeEvent(input$train, {
    # 1) make data
    dat <- make_data(
      n_obs   = input$nobs,
      noise_sd= input$noise,
      seed    = input$seed,
      alpha   = input$alpha,
      beta    = input$beta,
      period  = input$period,
      t_max   = 200
    )
    rv$df_truth <- dat$truth
    rv$df_obs   <- dat$obs
    
    # 2) train NN
    nn <- train_nn(
      df_truth = rv$df_truth,
      df_obs   = rv$df_obs,
      period   = input$period,
      epochs   = input$epochs,
      lr       = input$lr,
      H        = input$H,
      seed     = input$seed
    )
    rv$nn <- nn
    
    # 3) train PINN
    pinn <- train_pinn(
      df_truth = rv$df_truth,
      df_obs   = rv$df_obs,
      period   = input$period,
      alpha    = input$alpha,
      beta     = input$beta,
      lambda_phys = input$lambda,
      epochs   = input$epochs,
      lr       = input$lr,
      H        = input$H,
      seed     = input$seed
    )
    rv$pinn <- pinn
  })
  
  
  output$plot_fit <- renderPlot({
    shiny::validate(shiny::need(!is.null(rv$df_truth), "Click 'Generate data & train'."))
    df_truth <- rv$df_truth
    df_obs   <- rv$df_obs
    
    df_nn   <- if (!is.null(rv$nn))  dplyr::mutate(rv$nn$pred,  model = "NN (data-only)") else NULL
    df_pinn <- if (!is.null(rv$pinn)) dplyr::mutate(rv$pinn$pred, model = "PINN (data+physics)") else NULL
    df_fit  <- dplyr::bind_rows(df_nn, df_pinn)
    
    if (is.null(df_fit)) {
      return(
        ggplot() +
          geom_line(aes(t, y), data = df_truth, linewidth = 0.9, alpha = 0.6, linetype = "dashed") +
          geom_point(aes(t, y), data = df_obs, size = 2) +
          labs(title="Plain NN (blue) vs PINN (red) on sparse seasonal data", x="Day", y="Signal y(t)",
               caption="Dashed = ODE truth; points = noisy observations") +
          theme_minimal(base_size = 13)
      )
    }
    
    
    df_fit$model <- factor(
      df_fit$model,
      levels = c("NN (data-only)", "PINN (data+physics)"),
      labels = c("NN (data-only)", "PINN (data+physics)")
    )
    
    ggplot(df_fit, aes(t, y, color = model)) +
      geom_line(linewidth = 1, show.legend = TRUE) +
      geom_line(aes(t, y), data = df_truth, inherit.aes = FALSE,
                linewidth = 0.9, alpha = 0.6, linetype = "dashed", color = "grey40",
                show.legend = FALSE) +
      geom_point(aes(t, y), data = df_obs, inherit.aes = FALSE, size = 2,
                 show.legend = FALSE) +
      scale_color_manual(
        name   = "Model",  
        breaks = c("NN (data-only)", "PINN (data+physics)"),
        labels = c("NN (data-only)", "PINN (data+physics)"),
        values = c("NN (data-only)" = "#1f77b4", "PINN (data+physics)" = "#d62728"),
        guide  = guide_legend(title.position = "top")   
      ) +
      labs(title="Plain NN (blue) vs PINN (red) on sparse seasonal data",
           x="Day", y="Signal y(t)",
           caption="Dashed = ODE truth; points = noisy observations") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "right",
        plot.margin = margin(t = 5.5, r = 60, b = 5.5, l = 5.5, unit = "pt"),  
        legend.title = element_text(color = "black"),
        legend.text  = element_text(color = "black")
      )
  })
  
}
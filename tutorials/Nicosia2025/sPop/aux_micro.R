micro_optimum <- function(observed, S, 
                          T_opt = 25.0, k_max = 2.0, 
                          K = 5.0, n = 1.0) {
  # Ensure numeric vector
  observed <- as.numeric(observed)
  
  # Determine sigma depending on whether S is function, vector, or scalar
  if (is.function(S)) {
    sigma <- S(observed)
  } else if (is.numeric(S) && length(S) == length(observed)) {
    sigma <- S
  } else if (is.numeric(S) && length(S) == 1) {
    sigma <- rep(S, length(observed))
  } else {
    stop("If S is a vector, it must match the length of observed,
or S can be a scalar or a function returning a vector.")
  }
  
  delta <- abs(observed - T_opt)
  k <- k_max * (delta^n) / (K^n + delta^n)
  
  lower <- observed - k * sigma
  upper <- observed + k * sigma
  
  # For each time point, pick T_opt if inside bounds, otherwise nearest bound
  optimum_adj <- ifelse(
    (T_opt >= lower) & (T_opt <= upper),
    T_opt,
    ifelse(abs(T_opt - lower) < abs(T_opt - upper), lower, upper)
  )
  
  return(optimum_adj)
}

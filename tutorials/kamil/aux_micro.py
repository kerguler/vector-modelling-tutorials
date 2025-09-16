import numpy as np

def micro_optimum(observed, S, T_opt=25.0, k_max=2.0, K=5.0, n=1.0):
    """
    Extract for each time point the most optimum temperature 
    within adaptive k standard deviations of the observed value,
    where k grows smoothly with distance from T_opt.

    Parameters
    ----------
    observed : np.ndarray
        1D array of observed temperatures.
    S : float, np.ndarray, or callable
        Standard deviation of the spatial variation.
    T_opt : float
        Optimum temperature (default 25.0 °C).
    k_max : float
        Maximum multiplier for sigma (default 2.0).
    K : float
        Half-saturation distance from T_opt (°C).
    n : float
        Hill coefficient (smoothness, default 1.0 = gentle curve).

    Returns
    -------
    np.ndarray
        Array of optimum-adjusted temperatures.
    """
    observed = np.asarray(observed)

    if callable(S):
        sigma = S(observed)
    elif isinstance(S, np.ndarray):
        if S.shape != observed.shape:
            raise ValueError("If S is array, it must match observed shape")
        sigma = S
    else:
        sigma = np.full_like(observed, S, dtype=float)

    delta = np.abs(observed - T_opt)
    k = k_max * (delta**n) / (K**n + delta**n)

    lower = observed - k * sigma
    upper = observed + k * sigma

    optimum_adj = np.where(
        (T_opt >= lower) & (T_opt <= upper),
        T_opt,
        np.where(np.abs(T_opt - lower) < np.abs(T_opt - upper), lower, upper)
    )

    return optimum_adj

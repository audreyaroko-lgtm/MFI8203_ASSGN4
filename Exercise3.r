# ============================================================================
# Exercise 3: Effect of Volatility
# ============================================================================

source("FD_common.R")
library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
S0 <- 50
K <- 50
r <- 0.05
T <- 1
Smax <- 150
N <- 100

sigma_vec <- c(0.2, 0.4, 0.8)

cat("\n========================================\n")
cat("EXERCISE 3: Effect of Volatility\n")
cat("========================================\n\n")

# ---- 1. Find minimum M for stability (explicit) ----
cat("1. Minimum M for explicit scheme stability\n")
cat("-------------------------------------------\n")

min_M <- numeric(length(sigma_vec))
for (idx in seq_along(sigma_vec)) {
    sigma <- sigma_vec[idx]
    cat(sprintf("\nσ = %.1f\n", sigma))
    exact <- black_scholes_put(S0, K, r, sigma, T)
    cat(sprintf("Analytical price at S0: %.6f\n", exact))

    # Search for minimum stable M
    M_candidates <- seq(50, 10000, by = 50)
    found <- FALSE
    for (M in M_candidates) {
        fd <- fd_put_option(S0, K, r, sigma, T, Smax, N, M, theta = 0)
        stable <- all(is.finite(fd$V0)) && all(fd$V0 >= -1e-8) && max(abs(fd$V0)) < 1e3
        if (stable) {
            min_M[idx] <- M
            found <- TRUE
            break
        }
    }
    if (found) {
        cat(sprintf("  Minimum M for stability: %d\n", min_M[idx]))
        cat(sprintf("  dt = T/M = %.5f\n", T / min_M[idx]))
    } else {
        cat("  No stable M found up to 10000.\n")
        min_M[idx] <- NA
    }
}

# CFL scaling
cat("\nCFL scaling (M_min vs σ²):\n")
cfl_df <- data.frame(
    sigma = sigma_vec,
    sigma2 = sigma_vec^2,
    M_min = min_M
)
print(cfl_df)
cat("Observation: M_min scales approximately as σ², as expected.\n")

# ---- 2. CN convergence for different sigma ----
cat("\n\n2. Crank-Nicolson convergence\n")
cat("-----------------------------\n")

M_cn_vals <- c(50, 100, 200, 500, 1000)
cn_results <- data.frame()

for (sigma in sigma_vec) {
    exact <- black_scholes_put(S0, K, r, sigma, T)
    for (M in M_cn_vals) {
        fd <- fd_put_option(S0, K, r, sigma, T, Smax, N, M, theta = 0.5)
        idx <- which.min(abs(fd$S - S0))
        err <- abs(fd$V0[idx] - exact)
        cn_results <- rbind(cn_results,
                            data.frame(sigma = sigma, M = M, Error = err))
    }
}

# Reshape for display
cn_table <- cn_results %>%
    pivot_wider(names_from = sigma, values_from = Error, names_prefix = "σ=")
cat("\nCN absolute errors at S0=50:\n")
print(cn_table, digits = 4)

# Convergence plot
p <- ggplot(cn_results, aes(x = M, y = Error, colour = factor(sigma))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = "Crank-Nicolson Convergence for Different Volatilities",
         x = "Number of time steps (M)", y = "Absolute error",
         colour = expression(sigma)) +
    theme_minimal() +
    theme(legend.position = "bottom")

dir.create("figures", showWarnings = FALSE)
ggsave("figures/ex3_plot.png", p, width = 8, height = 5, dpi = 150)
print(p)

cat("\nPlot saved to figures/ex3_plot.png\n")
cat("========================================\n")
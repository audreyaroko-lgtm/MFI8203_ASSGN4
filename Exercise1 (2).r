# ============================================================================
# Exercise 1: Comparing FD Schemes
# ============================================================================

source("FD_common.R")
library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
S0 <- 50
K <- 50
r <- 0.05
sigma <- 0.2
T <- 1
Smax <- 150
N <- 100

M_exp <- 5000
M_imp <- 100
M_cn  <- 100

cat("\n========================================\n")
cat("EXERCISE 1: Comparing FD Schemes\n")
cat("========================================\n\n")

# Analytical price at S0
exact_price <- black_scholes_put(S0, K, r, sigma, T)
cat(sprintf("Analytical put price at S0 = %.2f: %.6f\n\n", S0, exact_price))

# Run schemes
cat("Running explicit scheme (M =", M_exp, ") ...\n")
fd_exp <- fd_put_option(S0, K, r, sigma, T, Smax, N, M_exp, theta = 0)

cat("Running implicit scheme (M =", M_imp, ") ...\n")
fd_imp <- fd_put_option(S0, K, r, sigma, T, Smax, N, M_imp, theta = 1)

cat("Running Crank-Nicolson (M =", M_cn, ") ...\n")
fd_cn  <- fd_put_option(S0, K, r, sigma, T, Smax, N, M_cn,  theta = 0.5)

# Extract prices at S0
idx_S0 <- which.min(abs(fd_exp$S - S0))
prices <- c(
    Explicit = fd_exp$V0[idx_S0],
    Implicit = fd_imp$V0[idx_S0],
    CN       = fd_cn$V0[idx_S0]
)
errors <- abs(prices - exact_price)

# Table
results_df <- data.frame(
    Scheme = c("Explicit", "Implicit", "Crank-Nicolson"),
    Theta  = c(0, 1, 0.5),
    M      = c(M_exp, M_imp, M_cn),
    Price  = prices,
    Error  = errors
)

cat("\nResults at S0 = 50:\n")
print(results_df, digits = 6)

# Plot full solution curves
S_anal <- seq(0, Smax, length.out = 500)
V_exact <- black_scholes_put(S_anal, K, r, sigma, T)

plot_data <- data.frame(
    S = fd_exp$S,
    Exact = black_scholes_put(fd_exp$S, K, r, sigma, T),
    Explicit = fd_exp$V0,
    Implicit = fd_imp$V0,
    CN = fd_cn$V0
)

plot_long <- pivot_longer(plot_data,
                          cols = c(Exact, Explicit, Implicit, CN),
                          names_to = "Scheme",
                          values_to = "Price")

p <- ggplot(plot_long, aes(x = S, y = Price, colour = Scheme)) +
    geom_line(size = 0.9) +
    scale_colour_manual(values = c("Exact" = "black",
                                   "Explicit" = "blue",
                                   "Implicit" = "red",
                                   "CN" = "darkgreen")) +
    labs(title = "European Put Option: FD Schemes vs Analytical",
         subtitle = paste0("K=", K, ", r=", r, ", σ=", sigma, ", T=", T),
         x = "Stock Price S", y = "Option Value V(S,0)") +
    theme_minimal() +
    theme(legend.position = "bottom")

# Save plot
dir.create("figures", showWarnings = FALSE)
ggsave("figures/ex1_plot.png", p, width = 8, height = 5, dpi = 150)
print(p)

cat("\nPlot saved to figures/ex1_plot.png\n")
cat("========================================\n")
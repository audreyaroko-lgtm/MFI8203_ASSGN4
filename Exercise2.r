# ============================================================================
# Exercise 2: Stability Analysis (Explicit Scheme)
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

M_values <- c(50, 100, 500, 1000, 5000)

cat("\n========================================\n")
cat("EXERCISE 2: Stability Analysis\n")
cat("========================================\n\n")

exact_price <- black_scholes_put(S0, K, r, sigma, T)
cat(sprintf("Analytical put price at S0 = %.2f: %.6f\n\n", S0, exact_price))

results <- data.frame(M = integer(), Error = numeric(), Stable = logical())
fd_list <- list()

for (M in M_values) {
    cat(sprintf("Testing explicit scheme with M = %d ...\n", M))
    fd <- fd_put_option(S0, K, r, sigma, T, Smax, N, M, theta = 0)
    fd_list[[as.character(M)]] <- fd

    idx <- which.min(abs(fd$S - S0))
    price <- fd$V0[idx]
    err <- abs(price - exact_price)

    # Stability criteria: finite, non-negative (within tolerance), no blow-up
    stable <- all(is.finite(fd$V0)) &&
              all(fd$V0 >= -1e-8) &&
              !any(is.nan(fd$V0)) &&
              max(abs(fd$V0)) < 1e3

    results <- rbind(results, data.frame(M = M, Error = err, Stable = stable))
}

cat("\nStability results:\n")
print(results)

# Identify critical M
unstable <- results[!results$Stable, ]
critical <- if (nrow(unstable) > 0) max(unstable$M) else NA
if (!is.na(critical)) {
    cat(sprintf("\nCritical M (below which unstable): %d\n", critical))
    cat(sprintf("Corresponding dt = T/M = %.5f\n", T / critical))
} else {
    cat("\nAll tested M are stable.\n")
}

# Plot unstable (M=50) vs stable (M=5000)
fd_unstable <- fd_list[["50"]]
fd_stable   <- fd_list[["5000"]]

plot_data <- data.frame(
    S = fd_unstable$S,
    Exact = black_scholes_put(fd_unstable$S, K, r, sigma, T),
    Unstable = fd_unstable$V0,
    Stable = fd_stable$V0
)

plot_long <- pivot_longer(plot_data,
                          cols = c(Exact, Unstable, Stable),
                          names_to = "Case",
                          values_to = "Price")

p <- ggplot(plot_long, aes(x = S, y = Price, colour = Case)) +
    geom_line(size = 0.9) +
    scale_colour_manual(values = c("Exact" = "black",
                                   "Unstable" = "red",
                                   "Stable" = "blue")) +
    labs(title = "Stability of Explicit FD Scheme",
         subtitle = "M=50 (unstable) vs M=5000 (stable)",
         x = "Stock Price S", y = "Option Value V(S,0)") +
    theme_minimal() +
    theme(legend.position = "bottom")

dir.create("figures", showWarnings = FALSE)
ggsave("figures/ex2_plot.png", p, width = 8, height = 5, dpi = 150)
print(p)

# Verify implicit scheme stability
cat("\nVerifying implicit scheme stability for all M...\n")
imp_stable <- data.frame(M = integer(), Stable = logical())
for (M in M_values) {
    fd_imp <- fd_put_option(S0, K, r, sigma, T, Smax, N, M, theta = 1)
    stable <- all(is.finite(fd_imp$V0)) && all(fd_imp$V0 >= -1e-8)
    imp_stable <- rbind(imp_stable, data.frame(M = M, Stable = stable))
}
print(imp_stable)
cat("Implicit scheme is unconditionally stable (all TRUE).\n")

cat("\nPlot saved to figures/ex2_plot.png\n")
cat("========================================\n")
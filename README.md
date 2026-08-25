# MFI 8302 – Finite Difference Schemes for European Put Options

This repository contains the complete solution to the exercises on FD schemes for option pricing.

## Repository Structure

- `FD_common.R` – shared helper functions (Black–Scholes analytical price, θ‑method solver).
- `exercise1.R` – compares explicit, implicit, and Crank–Nicolson schemes (N=100, M=5000 for explicit, M=100 for others).
- `exercise2.R` – stability analysis: vary M for explicit scheme, find critical M, plot unstable vs stable.
- `exercise3.R` – effect of volatility (σ=0.4, 0.8): find minimum M for stability, analyse CN convergence.
- `report/` – LaTeX source for the final report.

## Requirements

- R (≥ 3.6)
- Required packages: `ggplot2`, `gridExtra`, `dplyr`, `tidyr`

Install missing packages with:
```r
install.packages(c("ggplot2", "gridExtra", "dplyr", "tidyr"))

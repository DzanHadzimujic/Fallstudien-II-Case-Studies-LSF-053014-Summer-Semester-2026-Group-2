### ===========================================================================
### EDUCATIONAL SCRIPT: AR(2) with Trend and Monthly Seasonality
###
### Purpose: Understand the FULL pipeline from raw non-stationary series
###          to forecasting and back-transformation to the original scale.
###
### Pipeline:
###   1.  Simulate a "passenger" series with KNOWN AR(2) + trend + seasonality
###   2.  Plot raw series — visually non-stationary
###   3.  ADF/KPSS tests on raw series — confirm non-stationarity
###   4.  Transform: log → seasonal diff (lag 12) → first diff
###   5.  ADF/KPSS on transformed series — confirm stationarity
###   6.  ACF/PACF → identify AR structure
###   7.  BIC order selection → should select p ≈ 2
###   8.  70/30 split: fit AR(p) on train, forecast on test (transformed scale)
###   9.  Back-transform forecasts to ORIGINAL passenger count scale
###  10.  MSFE on BOTH scales — see what it means for the business
###  11.  Interpretation section: why enterprise cares about original scale
### ===========================================================================

library(forecast)   # Arima(), forecast(), Acf(), Pacf()
library(tseries)    # adf.test(), kpss.test()

set.seed(123)

### ---------------------------------------------------------------------------
### 1. SIMULATE DATA (True DGP — in practice you never know this)
### ---------------------------------------------------------------------------
# We build: log(y_t) = level + trend + seasonal(month) + u_t
# where u_t ~ AR(2): u_t = phi1*u_{t-1} + phi2*u_{t-2} + eps_t
#
# This mimics a monthly passenger series:
#   - Slow upward trend (growing airline market)
#   - Peaks in January (holiday travel) and June (summer travel)
#   - AR(2) autocorrelation in the residuals (booking patterns carry over)

n       <- 156   # 13 years of monthly data (enough for 70/30 split + lags)
t_idx   <- 1:n
month_idx <- ((t_idx - 1) %% 12) + 1

# True AR(2) parameters — what we want BIC to recover
phi1      <- 0.65
phi2      <- 0.20
sigma_eps <- 0.05

# Monthly seasonal pattern (on log scale, centred at 0)
# Jan=1 high (holiday), Jun=6 high (summer), rest lower
seasonal_pattern <- c(
   0.40,  # Jan
   0.05,  # Feb
  -0.15,  # Mar
  -0.10,  # Apr
   0.05,  # May
   0.35,  # Jun
   0.10,  # Jul
  -0.05,  # Aug
  -0.15,  # Sep
  -0.10,  # Oct
   0.00,  # Nov
   0.20   # Dec
)
seas_component  <- seasonal_pattern[month_idx]
trend_component <- 0.012 * t_idx   # slow upward drift on log scale

# Generate AR(2) error process u_t
u   <- numeric(n)
eps <- rnorm(n, 0, sigma_eps)
for (i in 3:n) {
  u[i] <- phi1 * u[i - 1] + phi2 * u[i - 2] + eps[i]
}

# Combine into log-scale and exponentiate to get passenger counts
log_y_true <- 7 + trend_component + seas_component + u
y           <- round(exp(log_y_true))   # integer passenger counts

# Time axis: monthly starting Jan 2010
dates  <- seq(as.Date("2010-01-01"), by = "month", length.out = n)
ts_y   <- ts(y, start = c(2010, 1), frequency = 12)

cat("=== SIMULATED DATA SUMMARY ===\n")
cat("True AR(2) parameters: phi1 =", phi1, "  phi2 =", phi2, "\n")
cat("n =", n, "months |",
    format(dates[1], "%b %Y"), "to", format(dates[n], "%b %Y"), "\n")
cat("Passenger range:", min(y), "to", max(y), "\n\n")

### ---------------------------------------------------------------------------
### 2. PLOT RAW SERIES
### ---------------------------------------------------------------------------
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(dates, y, type = "l", col = "steelblue", lwd = 1,
     main = "Simulated Monthly Passenger Counts (raw)",
     xlab = "Date", ylab = "Passengers")
# Mark January and June peaks
jan_idx <- which(month_idx == 1)
jun_idx <- which(month_idx == 6)
points(dates[jan_idx], y[jan_idx], pch = 16, col = "red",   cex = 0.7)
points(dates[jun_idx], y[jun_idx], pch = 16, col = "orange", cex = 0.7)
legend("topleft", legend = c("Series", "January peak", "June peak"),
       col = c("steelblue","red","orange"), lty = c(1,NA,NA),
       pch = c(NA,16,16), bty = "n", cex = 0.8)

plot(dates, log(y), type = "l", col = "darkgreen", lwd = 1,
     main = "log(Passengers) — trend visible, seasonality visible",
     xlab = "Date", ylab = "log(Passengers)")

par(mfrow = c(1, 1))

### ---------------------------------------------------------------------------
### 3. CONFIRM NON-STATIONARITY ON RAW SERIES
### ---------------------------------------------------------------------------
# ADF  H0: unit root (non-stationary). p > 0.05 → fail to reject → non-stationary
# KPSS H0: stationary.                 p < 0.05 → reject H0   → non-stationary

cat("=== STATIONARITY TESTS ON RAW log(y) ===\n")
adf_raw  <- adf.test(log(y), alternative = "stationary")
kpss_raw <- kpss.test(log(y), null = "Level")
cat(sprintf("ADF  p = %.4f  → %s\n", adf_raw$p.value,
            ifelse(adf_raw$p.value  < 0.05, "stationary", "NON-STATIONARY ✗")))
cat(sprintf("KPSS p = %.4f  → %s\n\n", kpss_raw$p.value,
            ifelse(kpss_raw$p.value > 0.05, "stationary", "NON-STATIONARY ✗")))

### ---------------------------------------------------------------------------
### 4. TRANSFORM TO STATIONARITY
### ---------------------------------------------------------------------------
# Step 1: log(y)             — stabilises multiplicative variance
# Step 2: seasonal diff(12)  — removes periodic seasonal means
# Step 3: first diff         — removes remaining linear trend
#
# Result: z_t = (1-B)(1-B^12) log(y_t)
# This loses 12 + 1 = 13 observations at the start.

log_y    <- log(y)
ts_log_y <- ts(log_y, start = c(2010, 1), frequency = 12)

ts_z <- diff(diff(ts_log_y, lag = 12), differences = 1)
# ts_z[1] corresponds to original index 14 (January 2011)
z    <- as.numeric(ts_z)
n_z  <- length(z)
cat("Length after transformation:", n_z, "(lost 13 obs to differencing)\n\n")

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(ts_z, col = "purple", main = "Transformed series z_t = (1-B)(1-B^12) log(y_t)",
     ylab = "z_t")
abline(h = 0, lty = 2, col = "grey50")
plot(u[14:n], type = "l", col = "red",
     main = "True AR(2) errors u_t (for comparison — unknown in practice)",
     xlab = "Index", ylab = "u_t")
par(mfrow = c(1, 1))

### ---------------------------------------------------------------------------
### 5. CONFIRM STATIONARITY ON TRANSFORMED SERIES
### ---------------------------------------------------------------------------
cat("=== STATIONARITY TESTS ON TRANSFORMED z_t ===\n")
adf_z  <- adf.test(z, alternative = "stationary")
kpss_z <- kpss.test(z, null = "Level")
cat(sprintf("ADF  p = %.4f  → %s\n", adf_z$p.value,
            ifelse(adf_z$p.value  < 0.05, "stationary ✓", "NON-STATIONARY ✗")))
cat(sprintf("KPSS p = %.4f  → %s\n\n", kpss_z$p.value,
            ifelse(kpss_z$p.value > 0.05, "stationary ✓", "NON-STATIONARY ✗")))

### ---------------------------------------------------------------------------
### 6. ACF / PACF — IDENTIFY AR STRUCTURE
### ---------------------------------------------------------------------------
# For a pure AR(p): ACF decays geometrically, PACF cuts off after lag p.
# We expect the PACF to show significant spikes at lags 1 and 2 only.
#
# Note: differencing introduces MA components, so z_t is not a pure AR(2).
# However, with BIC we can still identify the dominant AR order.

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
Acf(ts_z,  lag.max = 24,
    main = "ACF of z_t\n(expect geometric decay for AR)")
Pacf(ts_z, lag.max = 24,
    main = "PACF of z_t\n(expect cutoff at p=2 for AR(2))")
par(mfrow = c(1, 1))

### ---------------------------------------------------------------------------
### 7. BIC ORDER SELECTION — SHOULD RECOVER p ≈ 2
### ---------------------------------------------------------------------------
# We search AR(0)..AR(12) with early stopping when BIC increases.

cat("=== AR ORDER SELECTION BY BIC (early stopping) ===\n")
max_p   <- 12
ic_vals <- rep(NA_real_, max_p + 1)
fits_ar <- vector("list", max_p + 1)
prev_ic <- Inf

for (p in 0:max_p) {
  fit <- tryCatch(
    Arima(ts_z, order = c(p, 0, 0), include.mean = TRUE, method = "ML"),
    error = function(e) NULL
  )
  fits_ar[[p + 1]] <- fit
  if (!is.null(fit)) {
    ic <- BIC(fit)
    ic_vals[p + 1] <- ic
    cat(sprintf("  AR(%2d)  BIC = %9.2f\n", p, ic))
    if (ic > prev_ic) {
      cat(sprintf("  → BIC increased at p=%d — stopping. Selected p=%d.\n\n",
                  p, p - 1))
      break
    }
    prev_ic <- ic
  }
}

best_p   <- which.min(ic_vals) - 1
best_fit <- fits_ar[[best_p + 1]]
cat(sprintf("True DGP:  AR(2) with phi1=%.2f, phi2=%.2f\n", phi1, phi2))
cat(sprintf("BIC chose: AR(%d)\n", best_p))
if (!is.null(best_fit)) {
  cat("Estimated coefficients:\n")
  print(round(coef(best_fit), 4))
}
cat("\n")

# Plot BIC curve
plot(0:max_p, ic_vals, type = "b", pch = 16, col = "steelblue",
     main = "BIC by AR order — early stopping rule",
     xlab = "p", ylab = "BIC", na.action = na.omit)
abline(v = best_p, col = "red", lty = 2)
text(best_p, min(ic_vals, na.rm = TRUE),
     labels = paste0("Selected\np=", best_p),
     pos = 4, col = "red", cex = 0.8)

### ---------------------------------------------------------------------------
### 8. TRAIN / TEST SPLIT AND FORECASTING ON TRANSFORMED SCALE
### ---------------------------------------------------------------------------
n_train <- floor(n_z * 0.80)
n_test  <- n_z - n_train

ts_z_train <- window(ts_z, end   = time(ts_z)[n_train])
ts_z_test  <- window(ts_z, start = time(ts_z)[n_train + 1])

# Fit AR(best_p) on training data only
fit_train <- Arima(ts_z_train,
                   order        = c(best_p, 0, 0),
                   include.mean = TRUE,
                   method       = "ML")

# h-step-ahead forecasts for the full test window
fc     <- forecast(fit_train, h = n_test)
z_hat  <- as.numeric(fc$mean)
z_act  <- as.numeric(ts_z_test)

msfe_z <- sqrt(mean((z_act - z_hat)^2))
cat(sprintf("=== TRANSFORMED SCALE MSFE ===\n"))
cat(sprintf("AR(%d) on z_t  |  train=%d  test=%d  |  MSFE = %.6f\n\n",
            best_p, n_train, n_test, msfe_z))

### ---------------------------------------------------------------------------
### 9. BACK-TRANSFORMATION TO ORIGINAL (PASSENGER COUNT) SCALE
### ---------------------------------------------------------------------------
# z_t = (1-B)(1-B^12) log(y_t)
#      = log(y_t) - log(y_{t-1}) - log(y_{t-12}) + log(y_{t-13})
#
# Rearranging:
#   log(y_t) = z_t + log(y_{t-1}) + log(y_{t-12}) - log(y_{t-13})
#
# The offset: ts_z[1] ↔ original index 14, so
#   ts_z train ends at original index: 13 + n_train
#   ts_z test  starts at             : 13 + n_train + 1

orig_offset <- 13          # values lost to differencing
train_end   <- orig_offset + n_train   # last training index in original y

# Reconstruct log_y recursively over the test period.
# We initialise with the true log_y (all n values) and overwrite test indices.
log_y_recon <- log_y   # length n; indices 1..n

for (h in seq_len(n_test)) {
  idx <- train_end + h    # index in original series (1-based)

  # The back-transform formula:
  log_y_recon[idx] <- z_hat[h] +
    log_y_recon[idx - 1]  +   # lag 1  (forecast value if h > 1)
    log_y_recon[idx - 12] -   # lag 12 (forecast value if h > 12)
    log_y_recon[idx - 13]     # lag 13 (forecast value if h > 13)
  # Note: for h=1..12, lags 12 and 13 fall in the training window → known.
  # For h > 12, they use previously reconstructed forecast values (recursive).
}

y_forecast  <- exp(log_y_recon[(train_end + 1):n])
y_actual    <- y[(train_end + 1):n]
msfe_orig   <- sqrt(mean((y_actual - y_forecast)^2))

cat(sprintf("=== ORIGINAL SCALE MSFE ===\n"))
cat(sprintf("Passenger count (back-transformed)  |  MSFE = %.2f passengers\n\n",
            msfe_orig))

### ---------------------------------------------------------------------------
### 10. PLOT: TRANSFORMED SCALE vs ORIGINAL SCALE FORECASTS
### ---------------------------------------------------------------------------
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

# --- Panel A: Transformed scale ---
t_z_all   <- time(ts_z)
t_z_test  <- t_z_all[n_train + seq_len(n_test)]

plot(t_z_all, as.numeric(ts_z),
     type = "l", col = "purple", lwd = 0.8,
     main = sprintf("Transformed scale z_t — AR(%d) forecast | MSFE = %.4f",
                    best_p, msfe_z),
     xlab = "Time", ylab = "z_t")
lines(t_z_test, z_hat, col = "orange", lwd = 1.5, lty = 2)

# PI shading
polygon(c(t_z_test, rev(t_z_test)),
        c(as.numeric(fc$lower[, 2]), rev(as.numeric(fc$upper[, 2]))),
        col = adjustcolor("orange", 0.15), border = NA)
polygon(c(t_z_test, rev(t_z_test)),
        c(as.numeric(fc$lower[, 1]), rev(as.numeric(fc$upper[, 1]))),
        col = adjustcolor("orange", 0.25), border = NA)

abline(v = t_z_all[n_train], col = "grey40", lty = 3)
abline(h = 0, col = "grey70", lty = 2)
legend("topright",
       legend = c("Actual z_t", "Forecast", "80% PI", "95% PI", "Train|Test"),
       col    = c("purple","orange", adjustcolor("orange",0.40),
                  adjustcolor("orange",0.20), "grey40"),
       lty    = c(1,2,NA,NA,3), pch = c(NA,NA,15,15,NA),
       pt.cex = 1.5, bty = "n", cex = 0.75)

# --- Panel B: Original scale (back-transformed) ---
dates_test <- dates[(train_end + 1):n]

plot(dates, y, type = "l", col = "steelblue", lwd = 0.8,
     main = sprintf("Original scale — Back-transformed forecast | MSFE = %.0f passengers",
                    msfe_orig),
     xlab = "Date", ylab = "Passengers")
lines(dates_test, y_forecast, col = "red", lwd = 1.5, lty = 2)
abline(v = dates[train_end], col = "grey40", lty = 3)
legend("topleft",
       legend = c("Actual passengers", "Forecast (back-transformed)", "Train|Test"),
       col = c("steelblue","red","grey40"),
       lty = c(1, 2, 3), bty = "n", cex = 0.8)

par(mfrow = c(1, 1))

### ---------------------------------------------------------------------------
### 11. INTERPRETATION — WHY THE ORIGINAL SCALE MATTERS FOR A BUSINESS
### ---------------------------------------------------------------------------
cat("=== INTERPRETATION ===\n\n")

cat("Q: Should I report MSFE on the transformed or original scale?\n")
cat("A: For MODEL SELECTION (choosing p, comparing models) → use transformed scale.\n")
cat("   The z_t series is what the AR model actually fits.\n")
cat("   MSFE on z_t tells you how well the model captures autocorrelation.\n\n")

cat("Q: Why does enterprise care about original-scale forecasts?\n")
cat("A: Decisions are made in real units:\n")
cat("   - Airline: 'How many seats to open for sale in June?' (need y_hat in passengers)\n")
cat("   - Finance: 'What is the expected revenue next quarter?' (need y_hat in euros)\n")
cat("   - Logistics: 'How many trucks do we need?' (need y_hat in shipments)\n")
cat("   MSFE on original scale =", round(msfe_orig), "passengers tells you directly\n")
cat("   how far off the forecast is in business-relevant units.\n\n")

cat("Q: How is the back-transformation done?\n")
cat("A: We applied: z_t = (1-B)(1-B^12) log(y_t)\n")
cat("   which means: log(y_t) = z_t + log(y_{t-1}) + log(y_{t-12}) - log(y_{t-13})\n")
cat("   so:          y_t = exp(z_hat_t) * y_{t-1} * y_{t-12} / y_{t-13}\n")
cat("   For h > 12 steps ahead, y_{t-12} is itself a forecast → propagates error.\n\n")

cat("Q: Why does forecast uncertainty grow fast at long horizons?\n")
cat("A: Two sources of compounding error in multi-step back-transformation:\n")
cat("   (1) AR forecast error in z_t grows with h (further from training data)\n")
cat("   (2) Recursive back-transform uses previously forecast y values as inputs,\n")
cat("       so errors in y_{T+1} feed into y_{T+13}, y_{T+25}, etc.\n\n")

cat("Q: Why is this 'not exactly doable in real life'?\n")
cat("A: The BIC order p was selected using the FULL series (including the test set).\n")
cat("   In real life you only know the past — a genuine real-time forecaster would\n")
cat("   re-select p with only training data available, and would re-estimate\n")
cat("   parameters as each new observation arrives (rolling window).\n")
cat("   This means our MSFE is optimistic (lower than a real out-of-sample MSFE).\n")

### 9. Task (b): AR(p) with IC-based order selection --------------------------
# We fit pure AR(p) models on the transformed stationary series produced above.
# p is selected by minimizing an information criterion (BIC by default).

select_ar_order <- function(y, max_p = 3, criterion = "BIC") {
  # Build a comparison table for AR orders p = 0,...,max_p.
  # Each row stores the selected information criterion for that fitted AR(p).
  # Early-stopping rule: as soon as IC(p+1) > IC(p) we select p and stop,
  # enforcing a monotone-decrease requirement on the IC path.
  out <- data.frame(p = 0:max_p, IC = NA_real_)
  fits <- vector("list", max_p + 1)
  
  prev_ic <- Inf   # IC at the previous (accepted) order
  
  for (p in 0:max_p) {
    # Fit a pure AR(p) model: y_t on lags y_{t-1},...,y_{t-p}.
    # If a model fails to converge, keep NULL and continue the search.
    fit <- tryCatch(
      Arima(y, order = c(p, 0, 0), include.mean = TRUE, method = "ML"),
      error = function(e) NULL
    )
    
    fits[[p + 1]] <- fit
    
    if (!is.null(fit)) {
      # Criterion used for order selection (BIC by default).
      ic <- if (criterion == "AIC") AIC(fit) else BIC(fit)
      out$IC[p + 1] <- ic
      
      # Early stop: IC increased for the previous order was the best.
      if (ic > prev_ic) break
      
      prev_ic <- ic
    }
  }
  
  # Best model = one with the smallest IC value (among those evaluated).
  best_idx <- which.min(out$IC)
  list(
    criterion = criterion,
    table = out,
    best_p = out$p[best_idx],
    best_fit = fits[[best_idx]]
  )
}

format_ar_equation <- function(fit, y_name = "y_t") {
  # Convert fitted AR coefficients into compact backshift notation:
  # (1 - a1*B - ... - ap*B^p) y_t = epsilon_t
  coefs <- coef(fit)
  ar_idx <- grep("^ar", names(coefs))
  
  if (length(ar_idx) == 0) {
    # AR(0): white-noise model.
    return(paste0("(1)", y_name, " = epsilon_t"))
  }
  
  a <- unname(coefs[ar_idx])
  terms <- character(length(a))
  for (i in seq_along(a)) {
    terms[i] <- sprintf("%+.4fB^%d", -a[i], i)
  }
  
  lhs <- paste0("(1 ", paste(terms, collapse = " "), ")", y_name)
  paste0(lhs, " = epsilon_t")
}

ar_cases <- select_ar_order(ts_cases_id, max_p = 3, criterion = "BIC")
ar_deaths <- select_ar_order(ts_deaths_id, max_p = 3, criterion = "BIC")
ar_recovered <- select_ar_order(ts_recovered_id, max_p = 3, criterion = "BIC")

cat("\nAR order selection (BIC):\n")
cat("Cases: best p =", ar_cases$best_p, "\n")
cat("Deaths: best p =", ar_deaths$best_p, "\n")
cat("Recovered: best p =", ar_recovered$best_p, "\n")

cat("\nCompact AR representation:\n")
cat("Cases:", format_ar_equation(ar_cases$best_fit), "\n")
cat("Deaths:", format_ar_equation(ar_deaths$best_fit), "\n")
cat("Recovered:", format_ar_equation(ar_recovered$best_fit), "\n")

# Save IC curves for order selection
png(filename = file.path(plot_dir, "05_ar_order_selection_bic.png"),
    width = 1400, height = 1200, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(ar_cases$table$p, ar_cases$table$IC,
     type = "b", pch = 16, col = "steelblue",
     main = "AR(p) order selection by BIC - Cases",
     xlab = "p", ylab = "BIC")
abline(v = ar_cases$best_p, col = "red", lty = 2)

plot(ar_deaths$table$p, ar_deaths$table$IC,
     type = "b", pch = 16, col = "firebrick",
     main = "AR(p) order selection by BIC - Deaths",
     xlab = "p", ylab = "BIC")
abline(v = ar_deaths$best_p, col = "red", lty = 2)

plot(ar_recovered$table$p, ar_recovered$table$IC,
     type = "b", pch = 16, col = "darkgreen",
     main = "AR(p) order selection by BIC - Recovered",
     xlab = "p", ylab = "BIC")
abline(v = ar_recovered$best_p, col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

cat("Saved AR order-selection plot to:",
    file.path(plot_dir, "05_ar_order_selection_bic.png"), "\n")

### 10. Task (c): In-sample forecasting with 70/30 train/test split -----------
#
# Procedure:
#   1. Split each transformed stationary series into 70 % training / 30 % test.
#   2. Fit AR(best_p) for the BIC-selected order from task b for on the training set.
#   3. Generate h-step-ahead point forecasts for the entire test horizon.
#   4. Compute MSFE = mean((actual - forecast)^2) over the test period.
#   5. Plot forecasts vs actual values.
#
# NOTE (think why this is not exactly doable in real life):
#   In practice, at the end of the training period you would not know the
#   optimal model order p in advance: it was chosen using the *full* sample,
#   which includes the test observations. A truly real-time forecaster would
#   have to re-select p and re-estimate parameters as each new observation
#   arrives (rolling or expanding window), without ever looking at future data.
#   Using a fixed p from a full-sample search is therefore an optimistic
#   (in-sample) exercise, not a genuine out-of-sample evaluation.

# Helper: split series, fit AR(p), forecast, compute MSFE -------
# Work on log1p(original) with d=1 (first difference) internal to Arima
forecast_ar <- function(y_ts, best_p, split = 0.80) {
  n       <- length(y_ts)
  n_train <- floor(n * split)
  n_test  <- n - n_train
  
  y_train <- window(y_ts, end   = time(y_ts)[n_train])
  y_test  <- window(y_ts, start = time(y_ts)[n_train + 1])
  
  # Fit ARIMA(best_p, 1, 0) on training data only
  # d=1 applies first difference internally; we do not pre-difference here
  fit_train <- Arima(y_train,
                     order        = c(best_p, 1, 0),
                     include.mean = FALSE,
                     method       = "ML")
  
  # h-step-ahead point forecasts for the full test window
  fc   <- forecast(fit_train, h = n_test)
  yhat <- as.numeric(fc$mean)
  yact <- as.numeric(y_test)
  
  msfe <- sqrt(mean((yact - yhat)^2))
  
  list(
    fit      = fit_train,
    fc       = fc,
    y_train  = y_train,
    y_test   = y_test,
    yhat     = yhat,
    yact     = yact,
    msfe     = msfe,
    n_train  = n_train,
    n_test   = n_test,
    time_all = time(y_ts),
    y_all    = as.numeric(y_ts)
  )
}

fc_cases     <- forecast_ar(log_cases,     ar_cases$best_p)
fc_deaths    <- forecast_ar(log_deaths,    ar_deaths$best_p)
fc_recovered <- forecast_ar(log_recovered, ar_recovered$best_p)

cat("\n--- Task (c): In-sample AR forecast MSFE (70/30 split, log1p scale) ---\n")
cat(sprintf("Cases     AR(%d) | train n=%d | test n=%d | MSFE = sqrt(MSE) = %.6f\n",
            ar_cases$best_p,     fc_cases$n_train,     fc_cases$n_test,     fc_cases$msfe))
cat(sprintf("Deaths    AR(%d) | train n=%d | test n=%d | MSFE = sqrt(MSE) = %.6f\n",
            ar_deaths$best_p,    fc_deaths$n_train,    fc_deaths$n_test,    fc_deaths$msfe))
cat(sprintf("Recovered AR(%d) | train n=%d | test n=%d | MSFE = sqrt(MSE) = %.6f\n",
            ar_recovered$best_p, fc_recovered$n_train, fc_recovered$n_test, fc_recovered$msfe))

# Helper: draw actual-vs-forecast panel for one series ------------------
plot_forecast_panel <- function(res, best_p, series_name, col_act, col_fc) {
  t_all  <- res$time_all
  t_test <- t_all[res$n_train + seq_len(res$n_test)]
  
  ylim <- range(c(res$yact, res$yhat,
                  as.numeric(res$fc$lower), as.numeric(res$fc$upper)),
                na.rm = TRUE)
  
  # Test actual series only
  plot(t_test, res$yact,
       type = "l", col = col_act, lwd = 0.8,
       ylim = ylim,
       main = sprintf("%s: AR(%d) in-sample forecast  |  MSFE = %.4f",
                      series_name, best_p, res$msfe),
       xlab = "Time index", ylab = "log1p(value)")
  
  # 80 % and 95 % prediction intervals (shaded)
  polygon(c(t_test, rev(t_test)),
          c(as.numeric(res$fc$lower[, 2]), rev(as.numeric(res$fc$upper[, 2]))),
          col = adjustcolor(col_fc, alpha.f = 0.15), border = NA)
  polygon(c(t_test, rev(t_test)),
          c(as.numeric(res$fc$lower[, 1]), rev(as.numeric(res$fc$upper[, 1]))),
          col = adjustcolor(col_fc, alpha.f = 0.25), border = NA)
  
  # Forecast line over test window
  lines(t_test, res$yhat, col = col_fc, lwd = 1.5, lty = 2)
  
  legend("topright",
         legend = c("Actual (test)", "Forecast", "80% PI", "95% PI"),
         col    = c(col_act, col_fc,
                    adjustcolor(col_fc, 0.40),
                    adjustcolor(col_fc, 0.25)),
         lty    = c(1, 2, NA, NA),
         pch    = c(NA, NA, 15, 15),
         pt.cex = 1.5,
         bty    = "n", cex = 0.75)
}

# Screen display
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot_forecast_panel(fc_cases,     ar_cases$best_p,     "Cases",     "steelblue", "orange")
plot_forecast_panel(fc_deaths,    ar_deaths$best_p,    "Deaths",    "firebrick", "purple")
plot_forecast_panel(fc_recovered, ar_recovered$best_p, "Recovered", "darkgreen", "darkorange")
par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "06_ar_insample_forecast.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot_forecast_panel(fc_cases,     ar_cases$best_p,     "Cases",     "steelblue", "orange")
plot_forecast_panel(fc_deaths,    ar_deaths$best_p,    "Deaths",    "firebrick", "purple")
plot_forecast_panel(fc_recovered, ar_recovered$best_p, "Recovered", "darkgreen", "darkorange")
par(mfrow = c(1, 1))
dev.off()

cat("Saved forecast plot to:", file.path(plot_dir, "06_ar_insample_forecast.png"), "\n")

### 11. Task (d): ARX with external predictors -------------------------------
# We now extend the pure AR model with exogenous predictors (xreg):
#   1) Age-group composition from RKI data (already available in the dataset)
#   2) Average daily temperature in Germany (Open-Meteo archive API)
#   3) Weekend dummy (reporting behavior effect)
#   4) Linear trend index (slow structural change not fully removed)

# 11.1 Build age-based predictors from the RKI dataset
covid_age <- covid_clean %>%
  mutate(
    is_60plus = grepl("^A(6|7|8|9)", Altersgruppe),
    is_80plus = grepl("^A8", Altersgruppe)
  )

daily_age_cases <- covid_age %>%
  group_by(Meldedatum) %>%
  summarise(
    cases_total = sum(AnzahlFall),
    cases_60plus = sum(AnzahlFall[is_60plus], na.rm = TRUE),
    share_cases_60plus = ifelse(cases_total > 0, cases_60plus / cases_total, 0),
    .groups = "drop"
  )

daily_age_deaths <- covid_age %>%
  filter(NeuerTodesfall %in% c(0L, 1L)) %>%
  group_by(Meldedatum) %>%
  summarise(
    deaths_total = sum(AnzahlTodesfall),
    deaths_80plus = sum(AnzahlTodesfall[is_80plus], na.rm = TRUE),
    share_deaths_80plus = ifelse(deaths_total > 0, deaths_80plus / deaths_total, 0),
    .groups = "drop"
  )

# 11.2 Download Germany daily mean temperature (source: Open-Meteo)
temp_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=51.1657&longitude=10.4515",
  "&start_date=", as.character(analysis_start),
  "&end_date=", as.character(analysis_end),
  "&daily=temperature_2m_mean",
  "&timezone=Europe%2FBerlin"
)

temp_json <- jsonlite::fromJSON(temp_url)

daily_temp <- data.frame(
  Meldedatum = as.Date(temp_json$daily$time),
  temp_c = as.numeric(temp_json$daily$temperature_2m_mean)
)

# If API has any gaps, fill smoothly for modeling continuity.
daily_temp <- daily_temp %>%
  arrange(Meldedatum) %>%
  mutate(
    temp_c = zoo::na.approx(temp_c, na.rm = FALSE),
    temp_c = zoo::na.locf(temp_c, na.rm = FALSE),
    temp_c = zoo::na.locf(temp_c, fromLast = TRUE, na.rm = FALSE)
  )

# 11.3 Build the final daily predictor table
predictors_daily <- data.frame(Meldedatum = all_dates$Meldedatum) %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  left_join(daily_age_cases %>% select(Meldedatum, share_cases_60plus),
            by = "Meldedatum") %>%
  left_join(daily_age_deaths %>% select(Meldedatum, share_deaths_80plus),
            by = "Meldedatum") %>%
  mutate(
    share_cases_60plus = replace(share_cases_60plus, is.na(share_cases_60plus), 0),
    share_deaths_80plus = replace(share_deaths_80plus, is.na(share_deaths_80plus), 0),
    weekday = weekdays(Meldedatum),
    is_weekend = as.integer(weekday %in% c("Saturday", "Sunday")),
    time_idx = as.numeric(Meldedatum - min(Meldedatum)) + 1
  )

cat("\nTask (d) predictors summary:\n")
cat("Temperature source: Open-Meteo Archive API\n")
cat("Mean temperature (C):", round(mean(predictors_daily$temp_c, na.rm = TRUE), 2), "\n")
cat("Mean share cases age 60+:", round(mean(predictors_daily$share_cases_60plus), 4), "\n")
cat("Mean share deaths age 80+:", round(mean(predictors_daily$share_deaths_80plus), 4), "\n")

# 11.4 Align exogenous variables with log-scale original series
# Since we now use log1p(y) with d=1 (not pre-differenced), xreg should
# match the original series length n directly (Arima handles differencing).

if (nrow(predictors_daily) != length(ts_cases)) {
  stop("Predictor table length mismatch with original series length.")
}

build_xreg <- function(cols) {
  as.matrix(predictors_daily[1:nrow(predictors_daily), cols, drop = FALSE])
}

xreg_cases <- build_xreg(c("temp_c", "is_weekend", "time_idx", "share_cases_60plus"))
xreg_deaths <- build_xreg(c("temp_c", "is_weekend", "time_idx", "share_deaths_80plus"))
xreg_recovered <- build_xreg(c("temp_c", "is_weekend", "time_idx", "share_cases_60plus"))

# 11.5 Fit ARX model and forecast (same 70/30 split as Task c)
# Work on log1p(original) with d=1; xreg matches original series length.
forecast_arx <- function(y_ts, xreg_mat, best_p, split = 0.80) {
  n <- length(y_ts)
  if (nrow(xreg_mat) != n) {
    stop("xreg rows must match original series length.")
  }
  
  n_train <- floor(n * split)
  n_test <- n - n_train
  
  y_train <- window(y_ts, end = time(y_ts)[n_train])
  y_test <- window(y_ts, start = time(y_ts)[n_train + 1])
  x_train <- xreg_mat[1:n_train, , drop = FALSE]
  x_test <- xreg_mat[(n_train + 1):n, , drop = FALSE]
  
  fit_train <- Arima(
    y_train,
    order = c(best_p, 1, 0),
    xreg = x_train,
    include.mean = FALSE,
    method = "ML"
  )
  
  fc <- forecast(fit_train, h = n_test, xreg = x_test)
  yhat <- as.numeric(fc$mean)
  yact <- as.numeric(y_test)
  
  msfe <- sqrt(mean((yact - yhat)^2))
  
  list(
    fit = fit_train,
    fc = fc,
    yhat = yhat,
    yact = yact,
    msfe = msfe,
    n_train = n_train,
    n_test = n_test,
    time_all = time(y_ts),
    y_all = as.numeric(y_ts)
  )
}

arx_cases <- forecast_arx(log_cases, xreg_cases, ar_cases$best_p)
arx_deaths <- forecast_arx(log_deaths, xreg_deaths, ar_deaths$best_p)
arx_recovered <- forecast_arx(log_recovered, xreg_recovered, ar_recovered$best_p)

cat("\n--- Task (d): ARX forecast MSFE (log1p scale) ---\n")
cat(sprintf("Cases     ARX(%d) | MSFE = %.6f\n", ar_cases$best_p, arx_cases$msfe))
cat(sprintf("Deaths    ARX(%d) | MSFE = %.6f\n", ar_deaths$best_p, arx_deaths$msfe))
cat(sprintf("Recovered ARX(%d) | MSFE = %.6f\n", ar_recovered$best_p, arx_recovered$msfe))

cat("\n--- Task (d): AR vs ARX (MSFE on log1p scale) ---\n")
cat(sprintf("Cases:     AR = %.6f | ARX = %.6f\n", fc_cases$msfe, arx_cases$msfe))
cat(sprintf("Deaths:    AR = %.6f | ARX = %.6f\n", fc_deaths$msfe, arx_deaths$msfe))
cat(sprintf("Recovered: AR = %.6f | ARX = %.6f\n", fc_recovered$msfe, arx_recovered$msfe))

cat("\nTask (d) selected ARX coefficients:\n")
cat("Cases coefficients:\n")
print(round(coef(arx_cases$fit), 4))
cat("Deaths coefficients:\n")
print(round(coef(arx_deaths$fit), 4))
cat("Recovered coefficients:\n")
print(round(coef(arx_recovered$fit), 4))

plot_arx_panel <- function(res, best_p, series_name, col_act, col_fc) {
  t_all  <- res$time_all
  t_test <- t_all[res$n_train + seq_len(res$n_test)]
  
  ylim <- range(c(res$yact, res$yhat,
                  as.numeric(res$fc$lower), as.numeric(res$fc$upper)),
                na.rm = TRUE)
  
  # Test actual series only
  plot(t_test, res$yact,
       type = "l", col = col_act, lwd = 0.8,
       ylim = ylim,
       main = sprintf("%s: ARX(%d) forecast  |  MSFE = %.4f",
                      series_name, best_p, res$msfe),
       xlab = "Time index", ylab = "log1p(value)")
  
  polygon(c(t_test, rev(t_test)),
          c(as.numeric(res$fc$lower[, 2]), rev(as.numeric(res$fc$upper[, 2]))),
          col = adjustcolor(col_fc, alpha.f = 0.15), border = NA)
  polygon(c(t_test, rev(t_test)),
          c(as.numeric(res$fc$lower[, 1]), rev(as.numeric(res$fc$upper[, 1]))),
          col = adjustcolor(col_fc, alpha.f = 0.25), border = NA)
  
  lines(t_test, res$yhat, col = col_fc, lwd = 1.5, lty = 2)
  
  legend("topright",
         legend = c("Actual (test)", "ARX forecast", "80% PI", "95% PI"),
         col = c(col_act, col_fc,
                 adjustcolor(col_fc, 0.40), adjustcolor(col_fc, 0.25)),
         lty = c(1, 2, NA, NA),
         pch = c(NA, NA, 15, 15),
         pt.cex = 1.5,
         bty = "n", cex = 0.75)
}

# Screen display
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot_arx_panel(arx_cases, ar_cases$best_p, "Cases", "steelblue", "orange")
plot_arx_panel(arx_deaths, ar_deaths$best_p, "Deaths", "firebrick", "purple")
plot_arx_panel(arx_recovered, ar_recovered$best_p, "Recovered", "darkgreen", "darkorange")
par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "07_arx_forecast_with_predictors.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot_arx_panel(arx_cases, ar_cases$best_p, "Cases", "steelblue", "orange")
plot_arx_panel(arx_deaths, ar_deaths$best_p, "Deaths", "firebrick", "purple")
plot_arx_panel(arx_recovered, ar_recovered$best_p, "Recovered", "darkgreen", "darkorange")
par(mfrow = c(1, 1))
dev.off()

cat("Saved ARX forecast plot to:",
    file.path(plot_dir, "07_arx_forecast_with_predictors.png"), "\n")


### 12. Task (e): Seasonal AR model  Phi(B^365) phi(B) y_t = epsilon_t ----------
#
# Here y_t is the ORIGINAL daily series (log1p-transformed for variance
# stabilisation). Stationarity and annual seasonality are handled entirely
# inside Arima() via d=1 (first difference) and D=1, period=365 (annual
# seasonal difference), so the model is SARIMA(p,1,0)(P,1,0)[365] on the
# log scale. This is equivalent to fitting Phi(B^365) phi(B) on the
# doubly-differenced series but works directly on the original data and
# avoids mixing the lag-7 weekly pre-differencing (used in tasks bford) with
# an annual seasonal AR structure.


# 12.1 Grid-search over (p, P) by BIC
# Model: SARIMA(p, 1, 0)(P, 1, 0)[365] on log1p(y)
select_sar_order <- function(y, max_p = 3, max_P = 3, season = 365) {
  best_bic <- Inf
  best_p   <- 1L
  best_P   <- 0L
  results  <- data.frame(p = integer(), P = integer(), BIC = numeric())
  
  for (p in 0:max_p) {
    for (P in 0:max_P) {
      if (p == 0 && P == 0) next        # need at least one AR term
      fit <- tryCatch(
        forecast::Arima(y,
                        order    = c(p, 1, 0),
                        seasonal = list(order = c(P, 1, 0), period = season),
                        method   = "CSS-ML",
                        include.mean = FALSE),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      bic_val <- BIC(fit)
      results <- rbind(results, data.frame(p = p, P = P, BIC = bic_val))
      if (bic_val < best_bic) {
        best_bic <- bic_val
        best_p   <- p
        best_P   <- P
      }
    }
  }
  list(best_p = best_p, best_P = best_P, best_bic = best_bic, table = results)
}

# 12.2 Format the seasonal AR model equation (backshift notation)
format_sar_equation <- function(fit, y_name, season = 365) {
  cf  <- coef(fit)
  ar_names  <- grep("^ar",   names(cf), value = TRUE)
  sar_names <- grep("^sar",  names(cf), value = TRUE)
  
  phi_str <- if (length(ar_names) == 0) {
    "1"
  } else {
    terms <- paste0(sprintf("%.4f", -cf[ar_names]), "B^", seq_along(ar_names))
    paste0("(1 - ", paste(terms, collapse = " - "), ")")
  }
  
  Phi_str <- if (length(sar_names) == 0) {
    "1"
  } else {
    terms <- paste0(sprintf("%.4f", -cf[sar_names]),
                    "B^", seq_along(sar_names) * season)
    paste0("(1 - ", paste(terms, collapse = " - "), ")")
  }
  
  cat(sprintf("\n%s  Phi(B^%d) phi(B) y_t = eps_t\n", y_name, season))
  cat(sprintf("  phi(B)     = %s\n", phi_str))
  cat(sprintf("  Phi(B^%d)  = %s\n", season, Phi_str))
}

# 12.3 Fit + forecast function (70/30 split, same as tasks c & d)
# Works on the log1p(original) series; d=1 and D=1 are applied internally.
forecast_sar <- function(y_ts, best_p, best_P, season = 365, split = 0.80) {
  n       <- length(y_ts)
  n_train <- floor(n * split)
  n_test  <- n - n_train
  
  y_train <- window(y_ts, end   = time(y_ts)[n_train])
  y_test  <- window(y_ts, start = time(y_ts)[n_train + 1])
  
  fit <- forecast::Arima(y_train,
                         order    = c(best_p, 1, 0),
                         seasonal = list(order = c(best_P, 1, 0), period = season),
                         method   = "CSS-ML",
                         include.mean = FALSE)
  
  fc      <- forecast::forecast(fit, h = n_test)
  yhat    <- as.numeric(fc$mean)
  yact    <- as.numeric(y_test)
  msfe    <- sqrt(mean((yact - yhat)^2))
  
  list(fit      = fit,
       fc       = fc,
       msfe     = msfe,
       yhat     = yhat,
       yact     = yact,
       y_all    = as.numeric(y_ts),
       n_train  = n_train,
       n_test   = n_test,
       time_all = as.numeric(time(y_ts)))
}

cat("\n=== Task (e): Seasonal AR for order selection via BIC ===\n")
cat("(Model: SARIMA(p,1,0)(P,1,0)[365] on log1p(original); grid p in 0..3, P in 0..3)\n")

sar_ord_cases     <- select_sar_order(log_cases,     max_p = 3, max_P = 3, season = 365)
sar_ord_deaths    <- select_sar_order(log_deaths,    max_p = 3, max_P = 3, season = 365)
sar_ord_recovered <- select_sar_order(log_recovered, max_p = 3, max_P = 3, season = 365)

cat(sprintf("\nCases:     best p=%d, P=%d  (BIC=%.2f)\n",
            sar_ord_cases$best_p,     sar_ord_cases$best_P,     sar_ord_cases$best_bic))
cat(sprintf("Deaths:    best p=%d, P=%d  (BIC=%.2f)\n",
            sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    sar_ord_deaths$best_bic))
cat(sprintf("Recovered: best p=%d, P=%d  (BIC=%.2f)\n",
            sar_ord_recovered$best_p, sar_ord_recovered$best_P, sar_ord_recovered$best_bic))

# 12.4 Print BIC tables
cat("\nBIC grid for Cases:\n");     print(sar_ord_cases$table)
cat("\nBIC grid for Deaths:\n");    print(sar_ord_deaths$table)
cat("\nBIC grid for Recovered:\n"); print(sar_ord_recovered$table)

# 12.5 Fit on full series and print equations
fit_sar_cases <- forecast::Arima(log_cases,
                                 order    = c(sar_ord_cases$best_p,     1, 0),
                                 seasonal = list(order = c(sar_ord_cases$best_P,     1, 0), period = 365),
                                 method   = "CSS-ML", include.mean = FALSE)
fit_sar_deaths <- forecast::Arima(log_deaths,
                                  order    = c(sar_ord_deaths$best_p,    1, 0),
                                  seasonal = list(order = c(sar_ord_deaths$best_P,    1, 0), period = 365),
                                  method   = "CSS-ML", include.mean = FALSE)
fit_sar_recovered <- forecast::Arima(log_recovered,
                                     order    = c(sar_ord_recovered$best_p, 1, 0),
                                     seasonal = list(order = c(sar_ord_recovered$best_P, 1, 0), period = 365),
                                     method   = "CSS-ML", include.mean = FALSE)

format_sar_equation(fit_sar_cases,     "Cases",     season = 365)
format_sar_equation(fit_sar_deaths,    "Deaths",    season = 365)
format_sar_equation(fit_sar_recovered, "Recovered", season = 365)

# 12.6 70/30 forecast & MSFE (on log1p scale)
sar_cases     <- forecast_sar(log_cases,     sar_ord_cases$best_p,     sar_ord_cases$best_P)
sar_deaths    <- forecast_sar(log_deaths,    sar_ord_deaths$best_p,    sar_ord_deaths$best_P)
sar_recovered <- forecast_sar(log_recovered, sar_ord_recovered$best_p, sar_ord_recovered$best_P)

cat("\n--- Task (e): Seasonal AR MSFE (log1p scale, test portion) ---\n")
cat(sprintf("Cases     SAR(%d,%d)_365 | MSFE = %.6f\n",
            sar_ord_cases$best_p,     sar_ord_cases$best_P,     sar_cases$msfe))
cat(sprintf("Deaths    SAR(%d,%d)_365 | MSFE = %.6f\n",
            sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    sar_deaths$msfe))
cat(sprintf("Recovered SAR(%d,%d)_365 | MSFE = %.6f\n",
            sar_ord_recovered$best_p, sar_ord_recovered$best_P, sar_recovered$msfe))

# All three models (AR, ARX, SAR) now use log1p scale with d=1 internal differencing,
# so MSFE values are directly comparable across all models.
cat("\n--- Model comparison: AR vs ARX vs SAR (MSFE all on log1p scale) ---\n")
cat(sprintf("Cases:     AR=%.6f | ARX=%.6f | SAR=%.6f\n",
            fc_cases$msfe,     arx_cases$msfe,     sar_cases$msfe))
cat(sprintf("Deaths:    AR=%.6f | ARX=%.6f | SAR=%.6f\n",
            fc_deaths$msfe,    arx_deaths$msfe,    sar_deaths$msfe))
cat(sprintf("Recovered: AR=%.6f | ARX=%.6f | SAR=%.6f\n",
            fc_recovered$msfe, arx_recovered$msfe, sar_recovered$msfe))

# 12.7 Plot helper
plot_sar_panel <- function(res, best_p, best_P, series_name, col_act, col_fc) {
  t_all  <- res$time_all
  t_test <- t_all[res$n_train + seq_len(res$n_test)]
  yact   <- res$y_all[res$n_train + seq_len(res$n_test)]
  ylim   <- range(c(yact, res$yhat,
                    as.numeric(res$fc$lower), as.numeric(res$fc$upper)),
                  na.rm = TRUE)
  
  # Test actual series only
  plot(t_test, yact,
       type = "l", col = col_act, lwd = 0.8,
       ylim = ylim,
       main = sprintf("%s: SAR(%d,%d)_365  |  MSFE = %.4f",
                      series_name, best_p, best_P, res$msfe),
       xlab = "Time index", ylab = "log1p(value)")
  
  polygon(c(t_test, rev(t_test)),
          c(as.numeric(res$fc$lower[, 2]), rev(as.numeric(res$fc$upper[, 2]))),
          col = adjustcolor(col_fc, alpha.f = 0.15), border = NA)
  polygon(c(t_test, rev(t_test)),
          c(as.numeric(res$fc$lower[, 1]), rev(as.numeric(res$fc$upper[, 1]))),
          col = adjustcolor(col_fc, alpha.f = 0.25), border = NA)
  
  lines(t_test, res$yhat, col = col_fc, lwd = 1.5, lty = 2)
  
  legend("topright",
         legend = c("Actual (test)", "SAR forecast", "80% PI", "95% PI"),
         col    = c(col_act, col_fc,
                    adjustcolor(col_fc, 0.40), adjustcolor(col_fc, 0.25)),
         lty    = c(1, 2, NA, NA),
         pch    = c(NA, NA, 15, 15),
         pt.cex = 1.5, bty = "n", cex = 0.75)
}

# Screen display
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot_sar_panel(sar_cases,     sar_ord_cases$best_p,     sar_ord_cases$best_P,     "Cases",     "steelblue", "orange")
plot_sar_panel(sar_deaths,    sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    "Deaths",    "firebrick", "purple")
plot_sar_panel(sar_recovered, sar_ord_recovered$best_p, sar_ord_recovered$best_P, "Recovered", "darkgreen", "darkorange")
par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "08_sar_forecast.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot_sar_panel(sar_cases,     sar_ord_cases$best_p,     sar_ord_cases$best_P,     "Cases",     "steelblue", "orange")
plot_sar_panel(sar_deaths,    sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    "Deaths",    "firebrick", "purple")
plot_sar_panel(sar_recovered, sar_ord_recovered$best_p, sar_ord_recovered$best_P, "Recovered", "darkgreen", "darkorange")
par(mfrow = c(1, 1))
dev.off()
cat("Saved seasonal AR forecast plot to:", file.path(plot_dir, "08_sar_forecast.png"), "\n")

#post model comparison, AR vs SAR
ts_ex <-window(sar_cases$y_all, start = time(sar_cases$y_all)[sar_cases$n_train + 1])
ts_ex[[1]]
plot(ts_ex, type = "l")
lines(sar_cases$yhat, col = "red", lty = 2)


err_ar  <- fc_cases$yact  - fc_cases$yhat
err_sar <- sar_cases$yact - sar_cases$yhat

max(abs(err_ar))
max(abs(err_sar))

c(
  ME_AR    = mean(err_ar),
  ME_SAR   = mean(err_sar),
  MAE_AR   = mean(abs(err_ar)),
  MAE_SAR  = mean(abs(err_sar)),
  RMSE_AR  = sqrt(mean(err_ar^2)),
  RMSE_SAR = sqrt(mean(err_sar^2))
)

# Direct paired comparison: positive => SAR has larger squared error
d <- err_sar^2 - err_ar^2
c(mean_diff_sq = mean(d), prop_sar_worse = mean(d > 0))

plot(err_ar^2, xlab = "AR squared error", ylab = "SAR squared error",
     main = "Squared errors: AR vs SAR (log1p scale)")
plot(err_sar^2, xlab = "AR squared error", ylab = "SAR squared error",
     main = "Squared errors: AR vs SAR (log1p scale)")

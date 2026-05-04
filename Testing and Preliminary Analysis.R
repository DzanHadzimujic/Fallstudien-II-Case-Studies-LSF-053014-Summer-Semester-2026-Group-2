### 5b. Stationarity tests on transformed series ------------------------------
# ADF  for€” H0: unit root (non-stationary).  Small p-value for†’ reject H0 for†’ stationary.
# KPSS for€” H0: stationary.                  Small p-value for†’ reject H0 for†’ non-stationary.
# We want: ADF p < 0.05  AND  KPSS p > 0.05  to conclude stationarity.

run_stationarity_tests <- function(x, name) {
  adf  <- adf.test(x,  alternative = "stationary")
  kpss <- kpss.test(x, null = "Level")
  cat(sprintf(
    "%-12s | ADF  p = %.4f (%s) | KPSS p = %.4f (%s)\n",
    name,
    adf$p.value,
    ifelse(adf$p.value  < 0.05, "stationary    ", "NON-stationary"),
    kpss$p.value,
    ifelse(kpss$p.value > 0.05, "stationary    ", "NON-stationary")
  ))
}

cat("\nStationarity tests on transformed series (log1p + seasonal diff lag 7 + first diff):\n")
cat(sprintf("%-12s | %-30s | %-30s\n", "Series", "ADF", "KPSS"))
cat(strrep("-", 75), "\n")
run_stationarity_tests(ts_cases_id,     "Cases")
run_stationarity_tests(ts_deaths_id,    "Deaths")
run_stationarity_tests(ts_recovered_id, "Recovered")


# Output folder for all plots
plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

### 6. Time series plots -------------------------------------------------------
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(daily_cases$Meldedatum, daily_cases$cases,
     type = "l", col = "steelblue", lwd = 0.8,
     main = "Daily Covid-19 Cases in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Cases")

plot(daily_deaths$Meldedatum, daily_deaths$deaths,
     type = "l", col = "firebrick", lwd = 0.8,
     main = "Daily Covid-19 Deaths in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Deaths")

plot(daily_recovered$Meldedatum, daily_recovered$recovered,
     type = "l", col = "darkgreen", lwd = 0.8,
     main = "Daily Covid-19 Recovered in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Recovered")

par(mfrow = c(1, 1))

# Save time series panel
png(filename = file.path(plot_dir, "01_time_series_levels_cases_deaths_recovered.png"),
    width = 1400, height = 1400, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(daily_cases$Meldedatum, daily_cases$cases,
     type = "l", col = "steelblue", lwd = 0.8,
     main = "Daily Covid-19 Cases in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Cases")

plot(daily_deaths$Meldedatum, daily_deaths$deaths,
     type = "l", col = "firebrick", lwd = 0.8,
     main = "Daily Covid-19 Deaths in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Deaths")

plot(daily_recovered$Meldedatum, daily_recovered$recovered,
     type = "l", col = "darkgreen", lwd = 0.8,
     main = "Daily Covid-19 Recovered in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Recovered")

par(mfrow = c(1, 1))
dev.off()

### 7. Periodogram (seasonality detection) -------------------------------------
# Detect dominant seasonal periods from spectral peaks.
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

s1<-spec.pgram(log1p(ts_cases),
               demean = TRUE,
               detrend = TRUE,
               taper = 0,
               main = paste("Periodogram - Cases | detected period:", season_cases, "days"))
1/s1$freq

spec.pgram(log1p(ts_deaths),
           demean = TRUE,
           detrend = TRUE,
           taper = 0,
           main = paste("Periodogram - Deaths | detected period:", season_deaths, "days"))

spec.pgram(log1p(ts_recovered),
           demean = TRUE,
           detrend = TRUE,
           taper = 0,
           main = paste("Periodogram - Recovered | detected period:", season_recovered, "days"))

par(mfrow = c(1, 1))

png(filename = file.path(plot_dir, "02_periodogram_detected_seasonality.png"),
    width = 1400, height = 1400, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

spec.pgram(log1p(ts_cases),
           demean = TRUE,
           detrend = TRUE,
           taper = 0,
           main = paste("Periodogram - Cases | detected period:", season_cases, "days"))

spec.pgram(log1p(ts_deaths),
           demean = TRUE,
           detrend = TRUE,
           taper = 0,
           main = paste("Periodogram - Deaths | detected period:", season_deaths, "days"))

spec.pgram(log1p(ts_recovered),
           demean = TRUE,
           detrend = TRUE,
           taper = 0,
           main = paste("Periodogram - Recovered | detected period:", season_recovered, "days"))

par(mfrow = c(1, 1))
dev.off()

### 8. ACF and PACF (model-identification scale) ------------------------------
# ACF/PACF are shown on transformed and differenced series, which is preferable
# for model identification compared to raw non-stationary levels.

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

Acf(ts_cases_id, lag.max = 60,
    main = paste("ACF for Cases: log1p + d1 + D", season_cases))
Pacf(ts_cases_id, lag.max = 60,
     main = paste("PACF for Cases: log1p + d1 + D", season_cases))

Acf(ts_deaths_id, lag.max = 60,
    main = paste("ACF for Deaths: log1p + d1 + D", season_deaths))
Pacf(ts_deaths_id, lag.max = 60,
     main = paste("PACF for Deaths: log1p + d1 + D", season_deaths))

Acf(ts_recovered_id, lag.max = 60,
    main = paste("ACF for Recovered: log1p + d1 + D", season_recovered))
Pacf(ts_recovered_id, lag.max = 60,
     main = paste("PACF for Recovered: log1p + d1 + D", season_recovered))

par(mfrow = c(1, 1))

# Save transformed ACF/PACF panel
png(filename = file.path(plot_dir, "03_acf_pacf_transformed_d1_detectedD.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

Acf(ts_cases_id, lag.max = 60,
    main = paste("ACF for Cases: log1p + d1 + D", season_cases))
Pacf(ts_cases_id, lag.max = 60,
     main = paste("PACF for Cases: log1p + d1 + D", season_cases))

Acf(ts_deaths_id, lag.max = 60,
    main = paste("ACF for Deaths: log1p + d1 + D", season_deaths))
Pacf(ts_deaths_id, lag.max = 60,
     main = paste("PACF for Deaths: log1p + d1 + D", season_deaths))

Acf(ts_recovered_id, lag.max = 60,
    main = paste("ACF for Recovered: log1p + d1 + D", season_recovered))
Pacf(ts_recovered_id, lag.max = 60,
     main = paste("PACF for Recovered: log1p + d1 + D", season_recovered))

par(mfrow = c(1, 1))
dev.off()

# Save transformed level plot for variance-stabilisation discussion
png(filename = file.path(plot_dir, "04_time_series_log1p_cases_deaths_recovered.png"),
    width = 1400, height = 1400, res = 150)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))

plot(daily_cases$Meldedatum, log1p(daily_cases$cases),
     type = "l", col = "steelblue", lwd = 0.8,
     main = "Daily Covid-19 Cases (log1p scale)",
     xlab = "Date", ylab = "log1p(Cases)")

plot(daily_deaths$Meldedatum, log1p(daily_deaths$deaths),
     type = "l", col = "firebrick", lwd = 0.8,
     main = "Daily Covid-19 Deaths (log1p scale)",
     xlab = "Date", ylab = "log1p(Deaths)")

plot(daily_recovered$Meldedatum, log1p(daily_recovered$recovered),
     type = "l", col = "darkgreen", lwd = 0.8,
     main = "Daily Covid-19 Recovered (log1p scale)",
     xlab = "Date", ylab = "log1p(Recovered)")

par(mfrow = c(1, 1))
dev.off()

cat("\nSaved plots to:", plot_dir, "\n")
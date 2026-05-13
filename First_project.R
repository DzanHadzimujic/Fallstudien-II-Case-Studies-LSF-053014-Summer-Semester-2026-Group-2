### ===========================================================================
### Case Studies – Project 1: Covid Forecasting with Linear Models
### Task (a): Data import, cleaning, and time series exploration (ACF/PACF)
### Try to see if there is a day of the week that may have relation  
### ===========================================================================

### 1. Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(forecast)   # for Acf(), Pacf(), and later Arima()
library(zoo)        # for as.Date / time series helpers
library(patchwork)  # for combining ggplots side by side
library(tseries)    # for adf.test() and kpss.test()

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "Package 'jsonlite' is required for temperature download. ",
    "Install once with: install.packages('jsonlite')"
  )
}

### 1.1 Import the data with URL
setwd("C:/Users/alvar/Documents/CaseStudies/First_Assignment/")

# Specify the URL of the file you want to download
url <- "https://media.githubusercontent.com/media/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_SarsCov2_Infektionen.csv"
# Specify the file name and location where you want to save the file on your computer
file_name <- "Aktuell_Deutschland_SarsCov2_Infektionen.csv"
file_path <- file.path(getwd(), file_name)
# download.file() returns a status code (0 = success), not the data frame itself
options(timeout = 600)
download_status <- download.file(url, destfile = file_path, mode = "wb")
if (download_status != 0) {
  stop("Download failed. Check internet connection or URL.")
}

### 2. Import data -------------------------------------------------------------

# The file uses comma separator and UTF-8 encoding (per RKI documentation)
covid_raw <- read_csv(
  file_path,
  col_types = cols(
    IdLandkreis       = col_integer(),
    Altersgruppe      = col_character(),
    Geschlecht        = col_character(),
    Meldedatum        = col_date(format = "%Y-%m-%d"),
    Refdatum          = col_date(format = "%Y-%m-%d"),
    IstErkrankungsbeginn = col_integer(),
    NeuerFall         = col_integer(),
    NeuerTodesfall    = col_integer(),
    NeuGenesen        = col_integer(),
    AnzahlFall        = col_integer(),
    AnzahlTodesfall   = col_integer(),
    AnzahlGenesen     = col_integer()
  )
)

# Restrict analysis window required by assignment
analysis_start <- as.Date("2020-04-01")
analysis_end   <- as.Date("2026-03-31")

covid_raw <- covid_raw %>%
  filter(Meldedatum >= analysis_start, Meldedatum <= analysis_end)

cat("Raw data dimensions:", nrow(covid_raw), "rows x", ncol(covid_raw), "cols\n")
cat("Date range:", as.character(min(covid_raw$Meldedatum)),
    "to", as.character(max(covid_raw$Meldedatum)), "\n")
cat("\nNeuerFall values:\n"); print(table(covid_raw$NeuerFall))
cat("\nNeuerTodesfall values:\n"); print(table(covid_raw$NeuerTodesfall))
cat("\nMissing values per column:\n"); print(colSums(is.na(covid_raw)))

### 3. Clean data --------------------------------------------------------------
# Key cleaning logic (per RKI documentation):
#   NeuerFall  = -1 → case is being REMOVED (correction) → exclude from totals
#   NeuerFall  =  0 → established case (in today's and yesterday's snapshot)
#   NeuerFall  =  1 → newly added case
# → Keep NeuerFall %in% c(0, 1) for the case count series.
#
#   NeuerTodesfall = -9 → no death event in this group → exclude
#   NeuerTodesfall = -1 → death is being REMOVED (correction) → exclude
#   NeuerTodesfall =  0 → established death
#   NeuerTodesfall =  1 → newly reported death
# → Keep NeuerTodesfall %in% c(0, 1) for the death count series.
covid_raw %>%
  group_by(NeuerFall) %>%
  summarise(count = n()) %>%
  arrange(NeuerFall) %>%
  print()

covid_raw %>%
  group_by(NeuerTodesfall) %>%
  summarise(count = n(),
            deaths = sum(AnzahlTodesfall) 
  ) %>%
  arrange(NeuerTodesfall) %>%
  print()



nrow(covid_raw)  # should be more than cleaned
covid_clean <- covid_raw %>%
  filter(NeuerFall %in% c(0L, 1L))   # valid case records only
nrow(covid_clean)  # should be fewer than raw
# Negative AnzahlFall can appear for NeuerFall=-1 rows (which we already
# excluded), but guard against any stray negatives just in case.
cat("\nAny negative AnzahlFall after filter:", any(covid_clean$AnzahlFall < 0), "\n")
cat("Any negative AnzahlTodesfall after filter:",
    any(covid_clean$AnzahlTodesfall < 0), "\n")

### 4. Aggregate to daily Germany-wide series ---------------------------------
# Cases: sum AnzahlFall by Meldedatum (all age groups, genders, districts)
daily_cases <- covid_clean %>%
  group_by(Meldedatum) %>%
  summarise(cases = sum(AnzahlFall), .groups = "drop") %>%
  arrange(Meldedatum)

# Deaths: sum AnzahlTodesfall, but only for rows where a death was recorded
# (NeuerTodesfall %in% c(0,1) – not -9 "no death" and not -1 "correction")
daily_deaths <- covid_clean %>%
  filter(NeuerTodesfall %in% c(0L, 1L)) %>%
  group_by(Meldedatum) %>%
  summarise(deaths = sum(AnzahlTodesfall), .groups = "drop") %>%
  arrange(Meldedatum)

# Recovered: sum AnzahlGenesen where recovery status indicates valid records
# (NeuGenesen %in% c(0,1); excluding -9 and -1 correction rows)
daily_recovered <- covid_clean %>%
  filter(NeuGenesen %in% c(0L, 1L)) %>%
  group_by(Meldedatum) %>%
  summarise(recovered = sum(AnzahlGenesen), .groups = "drop") %>%
  arrange(Meldedatum)

# Fill in missing dates with 0 values
all_dates <- data.frame(Meldedatum = seq(min(daily_cases$Meldedatum),
                                          max(daily_cases$Meldedatum),
                                          by = "day"))

daily_cases  <- left_join(all_dates, daily_cases,  by = "Meldedatum") %>%
  mutate(cases  = replace(cases,  is.na(cases),  0L))
daily_deaths <- left_join(all_dates, daily_deaths, by = "Meldedatum") %>%
  mutate(deaths = replace(deaths, is.na(deaths), 0L))
daily_recovered <- left_join(all_dates, daily_recovered, by = "Meldedatum") %>%
  mutate(recovered = replace(recovered, is.na(recovered), 0L))

cat("\nDaily cases series: ", nrow(daily_cases), "days\n")
cat("Total cases in series:", sum(daily_cases$cases), "\n")
cat("Total deaths in series:", sum(daily_deaths$deaths), "\n")
cat("Total recovered in series:", sum(daily_recovered$recovered), "\n")
cat("Cases summary:\n");  print(summary(daily_cases$cases))
cat("Deaths summary:\n"); print(summary(daily_deaths$deaths))
cat("Recovered summary:\n"); print(summary(daily_recovered$recovered))

### 5. Create ts objects -------------------------------------------------------
start_date  <- min(daily_cases$Meldedatum)
start_year  <- as.integer(format(start_date, "%Y"))
start_yday  <- as.integer(format(start_date, "%j"))

# BoxCox() transforms a series. To estimate lambda only, use BoxCox.lambda().
# Add 1 so zero-count days are valid for transformation methods that need > 0.
lambda_cases <- BoxCox.lambda(daily_cases$cases + 1, method = "guerrero")
lambda_deaths <- BoxCox.lambda(daily_deaths$deaths + 1, method = "guerrero")

cat("\nEstimated Box-Cox lambdas (Guerrero method):\n")
cat(sprintf("Cases:  lambda = %.4f\n", lambda_cases))
cat(sprintf("Deaths: lambda = %.4f\n", lambda_deaths))

if (abs(lambda_cases) < 0.2 && abs(lambda_deaths) < 0.2) {
  cat("Interpretation: both lambdas are close to 0, so a log-type transform is justified.\n")
} else {
  cat("Interpretation: lambdas are not near 0 for both series; log1p is still admissible for comparability and handling zeros.\n")
}

# Daily ts with frequency = 365 (approximate – ignores leap years but standard
# practice for annual seasonality detection)
ts_cases  <- ts(daily_cases$cases,
                start     = c(start_year, start_yday),
                frequency = 365)

ts_deaths <- ts(daily_deaths$deaths,
                start     = c(start_year, start_yday),
                frequency = 365)

ts_recovered <- ts(daily_recovered$recovered,
                   start     = c(start_year, start_yday),
                   frequency = 365)


# Log-transform the original level series (variance stabilisation only)
log_cases     <- log1p(ts_cases)
log_deaths    <- log1p(ts_deaths)
log_recovered <- log1p(ts_recovered)

# Detect seasonal period from periodogram (math-based selection)
# peak_rank = 1 -> strongest peak, 2 -> second strongest peak, etc.
detect_top_periods <- function(series, min_period = 2, max_period = 600, n_peaks = 2) {
  # Compute periodogram on log scale to stabilize variance before spectral analysis.
  spec_obj <- spec.pgram(log1p(series),
                         plot = FALSE,
                         demean = TRUE,
                         detrend = TRUE,
                         taper = 0)

  # Convert spectral frequency to period in days:
  # period = (observations per year) / (cycles per year).
  periods <- frequency(series) / spec_obj$freq
  keep <- periods >= min_period & periods <= max_period

  # Fallback if requested period window contains no frequencies.
  if (!any(keep)) {
    return(7L)
  }

  # Keep only the selected period window.
  kept_spec <- spec_obj$spec[keep]
  kept_periods <- periods[keep]

  # Prefer local maxima to avoid selecting neighboring bins from one broad peak.
  n <- length(kept_spec)
  local_max <- rep(FALSE, n)
  if (n == 1) {
    local_max[1] <- TRUE
  } else {
    local_max[1] <- kept_spec[1] > kept_spec[2]
    local_max[n] <- kept_spec[n] > kept_spec[n - 1]
    if (n > 2) {
      for (i in 2:(n - 1)) {
        local_max[i] <- kept_spec[i] > kept_spec[i - 1] && kept_spec[i] > kept_spec[i + 1]
      }
    }
  }

  peak_candidates <- which(local_max)
  # If no strict local maxima exist, rank all bins by spectral power.
  if (length(peak_candidates) == 0) {
    peak_candidates <- seq_len(n)
  }

  # Rank candidate peaks by spectral density from strongest to weakest.
  ranked <- peak_candidates[order(kept_spec[peak_candidates], decreasing = TRUE)]
  # Round to integer days and enforce a minimum period of 2 days.
  ranked_periods <- as.integer(pmax(2, round(kept_periods[ranked])))

  # Keep distinct rounded periods so rank 2 is not the same cycle repeated.
  distinct_periods <- unique(ranked_periods)

  # Return top n requested distinct periodicities.
  n_use <- min(max(1L, as.integer(n_peaks)), length(distinct_periods))
  distinct_periods[seq_len(n_use)]
}

detect_seasonal_period <- function(series, min_period = 2, max_period = 600, peak_rank = 2) {
  # Reuse ranked periods and return the requested rank:
  # peak_rank = 1 (primary), 2 (secondary), etc.
  ranked_periods <- detect_top_periods(
    series,
    min_period = min_period,
    max_period = max_period,
    n_peaks = peak_rank
  )

  # Clamp to available output length for safety.
  ranked_periods[min(length(ranked_periods), max(1L, as.integer(peak_rank)))]
}

season_cases <- detect_seasonal_period(ts_cases, peak_rank = 2)
season_deaths <- detect_seasonal_period(ts_deaths, peak_rank = 2)
season_recovered <- detect_seasonal_period(ts_recovered, peak_rank = 2)

peaks_cases <- detect_top_periods(ts_cases, n_peaks = 2)
peaks_deaths <- detect_top_periods(ts_deaths, n_peaks = 2)
peaks_recovered <- detect_top_periods(ts_recovered, n_peaks = 2)

cat("\nTop periodicities (days) from periodogram:\n")
cat(sprintf("Cases:     primary = %d | secondary = %d\n", peaks_cases[1], peaks_cases[min(2, length(peaks_cases))]))
cat(sprintf("Deaths:    primary = %d | secondary = %d\n", peaks_deaths[1], peaks_deaths[min(2, length(peaks_deaths))]))
cat(sprintf("Recovered: primary = %d | secondary = %d\n", peaks_recovered[1], peaks_recovered[min(2, length(peaks_recovered))]))

cat("\nDetected seasonal period (days) - cases:", season_cases, "\n")
cat("Detected seasonal period (days) - deaths:", season_deaths, "\n")
cat("Detected seasonal period (days) - recovered:", season_recovered, "\n")

# Differencing diagnostics: compare strategies and inspect residual autocorrelation.
# Note: strong residual ACF after heavy differencing is common when over-differencing
# and then fitting AR-only models, because differencing can induce MA structure.
acf_diag <- function(x, label) {
  x_num <- as.numeric(x)
  a <- stats::acf(x_num, lag.max = 400, plot = FALSE)$acf
  acf1 <- a[2]
  acf7 <- if (length(a) >= 8) a[8] else NA_real_
  acf14 <- if (length(a) >= 15) a[15] else NA_real_
  acf365 <- if (length(a) >= 366) a[366] else NA_real_
  lb40 <- Box.test(x_num, lag = 40, type = "Ljung-Box")$p.value
  cat(sprintf(
    "%s | ACF(1)=%.3f ACF(7)=%.3f ACF(14)=%.3f ACF(365)=%.3f | Ljung-Box p(40)=%.4f\n",
    label, acf1, acf7, acf14, acf365, lb40
  ))
}

cases_d1_D7 <- diff(diff(log1p(ts_cases), lag = 7), differences = 1)
cases_d1_D365 <- diff(diff(log1p(ts_cases), lag = 365), differences = 1)
cases_d1_D7_D365 <- diff(diff(diff(log1p(ts_cases), lag = 7), lag = 365), differences = 1)

deaths_d1_D7 <- diff(diff(log1p(ts_deaths), lag = 7), differences = 1)
deaths_d1_D365 <- diff(diff(log1p(ts_deaths), lag = 365), differences = 1)
deaths_d1_D7_D365 <- diff(diff(diff(log1p(ts_deaths), lag = 7), lag = 365), differences = 1)

cat("\nAutocorrelation diagnostics by differencing strategy:\n")
acf_diag(cases_d1_D7, "Cases d=1 + D7")
acf_diag(cases_d1_D365, "Cases d=1 + D365")
acf_diag(cases_d1_D7_D365, "Cases d=1 + D7 + D365")
acf_diag(deaths_d1_D7, "Deaths d=1 + D7")
acf_diag(deaths_d1_D365, "Deaths d=1 + D365")
acf_diag(deaths_d1_D7_D365, "Deaths d=1 + D7 + D365")

# For tasks (b)-(d), use weekly seasonal differencing + first differencing.
# Annual seasonality is modeled explicitly later in task (e) via SARIMA with period=365.
ts_cases_id <- cases_d1_D7
ts_deaths_id <- deaths_d1_D7

# Visual check: transformed series after trend/seasonality removal
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(ts_cases_id,
  type = "l", col = "steelblue", lwd = 0.8,
  main = "Cases: after seasonal + trend differencing",
  xlab = "Time", ylab = "Transformed value")
abline(h = 0, col = "gray50", lty = 2)

plot(ts_deaths_id,
  type = "l", col = "firebrick", lwd = 0.8,
  main = "Deaths: after seasonal + trend differencing",
  xlab = "Time", ylab = "Transformed value")
abline(h = 0, col = "gray50", lty = 2)

par(mfrow = c(1, 1))

transformed_plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(transformed_plot_dir)) {
  dir.create(transformed_plot_dir, recursive = TRUE)
}

png(filename = file.path(transformed_plot_dir, "02c_transformed_series_after_differencing.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(ts_cases_id,
  type = "l", col = "steelblue", lwd = 0.8,
  main = "Cases: after seasonal + trend differencing",
  xlab = "Time", ylab = "Transformed value")
abline(h = 0, col = "gray50", lty = 2)

plot(ts_deaths_id,
  type = "l", col = "firebrick", lwd = 0.8,
  main = "Deaths: after seasonal + trend differencing",
  xlab = "Time", ylab = "Transformed value")
abline(h = 0, col = "gray50", lty = 2)

par(mfrow = c(1, 1))
dev.off()




### 5b. Stationarity tests on transformed series ------------------------------
# ADF  — H0: unit root (non-stationary).  Small p-value → reject H0 → stationary.
# KPSS — H0: stationary.                  Small p-value → reject H0 → non-stationary.
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

library(broom)
library(knitr)


adf_cases  <- adf.test(ts_cases_id,  alternative = "stationary")
adf_deaths <- adf.test(ts_deaths_id, alternative = "stationary")

stat_table <- data.frame(
  Series     = c("Cases", "Deaths"),
  Statistic  = round(c(adf_cases$statistic,  adf_deaths$statistic), 3),
  `Lag order` = c(adf_cases$parameter,  adf_deaths$parameter),
  `p-value`  = c(adf_cases$p.value,  adf_deaths$p.value),
  Decision   = c(
    ifelse(adf_cases$p.value  < 0.05, "Reject $H_0$", "Fail to reject"),
    ifelse(adf_deaths$p.value < 0.05, "Reject $H_0$", "Fail to reject")
  ),
  check.names = FALSE
)

cat(knitr::kable(stat_table, format = "latex", booktabs = TRUE,
  caption = "ADF stationarity tests on transformed series (log1p + $\\Delta_1$ + $\\Delta_7$)",
  label   = "tab:stationarity",
  escape  = FALSE))

# Output folder for all plots
plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

### 6. Time series plots -------------------------------------------------------
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(daily_cases$Meldedatum, daily_cases$cases,
     type = "l", col = "steelblue", lwd = 0.8,
     main = "Daily Covid-19 Cases in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Cases")

plot(daily_deaths$Meldedatum, daily_deaths$deaths,
     type = "l", col = "firebrick", lwd = 0.8,
     main = "Daily Covid-19 Deaths in Germany (by Meldedatum)",
     xlab = "Date", ylab = "Deaths")

par(mfrow = c(1, 1))

# Save time series panel
png(filename = file.path(plot_dir, "01_time_series_levels_cases_deaths.png"),
  width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(daily_cases$Meldedatum, daily_cases$cases,
  type = "l", col = "steelblue", lwd = 0.8,
  main = "Daily Covid-19 Cases in Germany (by Meldedatum)",
  xlab = "Date", ylab = "Cases")

plot(daily_deaths$Meldedatum, daily_deaths$deaths,
  type = "l", col = "firebrick", lwd = 0.8,
  main = "Daily Covid-19 Deaths in Germany (by Meldedatum)",
  xlab = "Date", ylab = "Deaths")

par(mfrow = c(1, 1))
dev.off()

### 7. Periodogram (seasonality detection) -------------------------------------
# Detect dominant seasonal periods from spectral peaks.
# Compute periodograms and convert frequency to period (days) for visualization.
s1 <- spec.pgram(log1p(ts_cases),
       demean = TRUE,
       detrend = TRUE,
       taper = 0,
       plot = FALSE)

s2 <- spec.pgram(log1p(ts_deaths),
       demean = TRUE,
       detrend = TRUE,
       taper = 0,
       plot = FALSE)

# Screen display: period-based periodograms with seasonal annotations
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

# Cases: find max within visible range (2-600 days)
periods_s1 <- 365 / s1$freq
keep_s1 <- periods_s1 >= 2 & periods_s1 <= 600
max_idx_s1 <- which.max(s1$spec[keep_s1])
actual_idx_s1 <- which(keep_s1)[max_idx_s1]
max_period_cases <- periods_s1[actual_idx_s1]

plot(periods_s1, s1$spec,
  type = "p", pch = 16, cex = 0.9, col = "steelblue",
     xlim = c(2, 600), ylim = c(0, max(s1$spec[keep_s1], na.rm = TRUE) * 1.1),
     xlab = "Period (days)", ylab = "Spectral Density",
     main = paste("Periodogram of Cases | detected periods:", 
                  paste(peaks_cases, collapse = ", "), "days"))
abline(v = season_cases, col = "orange", lty = 2, lwd = 1.5)
abline(v = max_period_cases, col = "red", lty = 2, lwd = 1.5)
grid(col = "gray70")

# Deaths: find max within visible range (2-600 days)
periods_s2 <- 365 / s2$freq
keep_s2 <- periods_s2 >= 2 & periods_s2 <= 600
max_idx_s2 <- which.max(s2$spec[keep_s2])
actual_idx_s2 <- which(keep_s2)[max_idx_s2]
max_period_deaths <- periods_s2[actual_idx_s2]

plot(periods_s2, s2$spec,
  type = "p", pch = 16, cex = 0.9, col = "firebrick",
     xlim = c(2, 600), ylim = c(0, max(s2$spec[keep_s2], na.rm = TRUE) * 1.1),
     xlab = "Period (days)", ylab = "Spectral Density",
     main = paste("Periodogram of Deaths | detected periods:", 
                  paste(peaks_deaths, collapse = ", "), "days"))
abline(v = season_deaths, col = "orange", lty = 2, lwd = 1.5)
abline(v = max_period_deaths, col = "red", lty = 2, lwd = 1.5)
grid(col = "gray70")

par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "02_periodogram_detected_seasonality.png"),
  width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

# Cases: find max within visible range (2-600 days)
plot(periods_s1, s1$spec,
  type = "p", pch = 16, cex = 0.9, col = "steelblue",
     xlim = c(2, 600), ylim = c(0, max(s1$spec[keep_s1], na.rm = TRUE) * 1.1),
     xlab = "Period (days)", ylab = "Spectral Density",
     main = paste("Periodogram of Cases | detected periods:", 
                  paste(peaks_cases, collapse = ", "), "days"))
abline(v = season_cases, col = "orange", lty = 2, lwd = 1.5)
abline(v = max_period_cases, col = "red", lty = 2, lwd = 1.5)
grid(col = "gray70")

# Deaths: find max within visible range (2-600 days)
plot(periods_s2, s2$spec,
  type = "p", pch = 16, cex = 0.9, col = "firebrick",
     xlim = c(2, 600), ylim = c(0, max(s2$spec[keep_s2], na.rm = TRUE) * 1.1),
     xlab = "Period (days)", ylab = "Spectral Density",
     main = paste("Periodogram of Deaths | detected periods:", 
                  paste(peaks_deaths, collapse = ", "), "days"))
abline(v = season_deaths, col = "orange", lty = 2, lwd = 1.5)
abline(v = max_period_deaths, col = "red", lty = 2, lwd = 1.5)
grid(col = "gray70")

par(mfrow = c(1, 1))
dev.off()

### 8. ACF/PACF on log-level series (seasonality visibility) ------------------
# For task (a), inspect correlation on log1p levels BEFORE differencing.
# Seasonal peaks around weekly lags should be visible here.
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

Acf(log_cases, lag.max = 60,
    main = "ACF - Cases (log1p levels)")
Pacf(log_cases, lag.max = 60,
     main = "PACF - Cases (log1p levels)")

Acf(log_deaths, lag.max = 60,
    main = "ACF - Deaths (log1p levels)")
Pacf(log_deaths, lag.max = 60,
     main = "PACF - Deaths (log1p levels)")

par(mfrow = c(1, 1))

png(filename = file.path(plot_dir, "02b_acf_pacf_log_levels.png"),
    width = 1600, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

Acf(log_cases, lag.max = 60,
    main = "ACF - Cases (log1p levels)")
Pacf(log_cases, lag.max = 60,
     main = "PACF - Cases (log1p levels)")

Acf(log_deaths, lag.max = 60,
    main = "ACF - Deaths (log1p levels)")
Pacf(log_deaths, lag.max = 60,
     main = "PACF - Deaths (log1p levels)")

par(mfrow = c(1, 1))
dev.off()

### 8b. ACF and PACF (model-identification scale) -----------------------------
# ACF/PACF on transformed and differenced series for AR model identification.

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

Acf(ts_cases_id, lag.max = 60,
  main = paste("ACF – Cases: log1p + d1 + D", season_cases))
Pacf(ts_cases_id, lag.max = 60,
   main = paste("PACF – Cases: log1p + d1 + D", season_cases))

Acf(ts_deaths_id, lag.max = 60,
  main = paste("ACF – Deaths: log1p + d1 + D", season_deaths))
Pacf(ts_deaths_id, lag.max = 60,
   main = paste("PACF – Deaths: log1p + d1 + D", season_deaths))

par(mfrow = c(1, 1))

# Save transformed ACF/PACF panel
png(filename = file.path(plot_dir, "03_acf_pacf_transformed_d1_detectedD.png"),
  width = 1600, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

Acf(ts_cases_id, lag.max = 60,
  main = paste("ACF – Cases: log1p + d1 + D", season_cases))
Pacf(ts_cases_id, lag.max = 60,
   main = paste("PACF – Cases: log1p + d1 + D", season_cases))

Acf(ts_deaths_id, lag.max = 60,
  main = paste("ACF – Deaths: log1p + d1 + D", season_deaths))
Pacf(ts_deaths_id, lag.max = 60,
   main = paste("PACF – Deaths: log1p + d1 + D", season_deaths))

par(mfrow = c(1, 1))
dev.off()

# Save transformed level plot for variance-stabilisation discussion
png(filename = file.path(plot_dir, "04_time_series_log1p_cases_deaths.png"),
  width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(daily_cases$Meldedatum, log1p(daily_cases$cases),
     type = "l", col = "steelblue", lwd = 0.8,
     main = "Daily Covid-19 Cases (log1p scale)",
     xlab = "Date", ylab = "log1p(Cases)")

plot(daily_deaths$Meldedatum, log1p(daily_deaths$deaths),
     type = "l", col = "firebrick", lwd = 0.8,
     main = "Daily Covid-19 Deaths (log1p scale)",
     xlab = "Date", ylab = "log1p(Deaths)")

par(mfrow = c(1, 1))
dev.off()

cat("\nSaved plots to:", plot_dir, "\n")



plot(365/s1$freq,s1$spec,type="p",ylim=c(0,2),xlim=c(2,10),
xlab = "Period (days)",ylab = "Spectral Density", main = "Periodogram of log_cases")
abline(v = 7, col = "red", lty = 2)
abline(v = 3.5, col = "blue", lty = 2)


# label it
text(x = 4.5, y = 1.5,labels = "3.5 days cycle",pos = 1, col = "blue")
text(x = 8, y = 1.5, labels = "7 days cycle",pos = 1, col = "red")
grid()

plot(365/s1$freq, s1$spec,type = "p",xlim = c(2, 365*2),xlab = "Period (days)",
ylab = "Spectral Density",main = "Periodogram of log_cases")
abline(v = 365, col = "orange", lty = 2)
text(365, max(s1$spec, na.rm = TRUE) * 0.9,
"1-year cycle", col = "orange", pos = 4)
grid()



### 9. Task (b): AR(p) with IC-based order selection --------------------------
# We fit pure AR(p) models on the transformed stationary series produced above.
# p is selected by minimizing an information criterion (BIC by default).

select_ar_order <- function(y, max_p = 20, criterion = "BIC") {
  # Build a comparison table for AR orders p = 0,...,max_p.
  # Each row stores the selected information criterion for that fitted AR(p).
  # Stop at the first local increase in IC and keep the previous AR order.
  out <- data.frame(p = 0:max_p, IC = NA_real_)
  fits <- vector("list", max_p + 1)
  prev_ic <- NA_real_
  best_idx <- NA_integer_

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
      if (toupper(criterion) == "AIC") {
        ic <- AIC(fit)
      } else {
        ic <- BIC(fit)
      }
      out$IC[p + 1] <- ic

      # Early stopping: if IC increases, keep previous AR order.
      if (!is.na(prev_ic) && ic > prev_ic) {
        best_idx <- p
        break
      }

      prev_ic <- ic
      best_idx <- p + 1
    }
  }

  # Fallback: if all fits failed, keep global minimum among evaluated entries.
  if (is.na(best_idx)) {
    best_idx <- which.min(out$IC)
  }

  best_p <- out$p[best_idx]
  best_fit <- fits[[best_idx]]

  # Keep only evaluated models in the output table (drop trailing/interior NAs).
  out <- out[!is.na(out$IC), , drop = FALSE]

  list(
    criterion = criterion,
    table = out,
    best_p = best_p,
    best_fit = best_fit
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


ar_cases <- select_ar_order(ts_cases_id, max_p = 20, criterion = "BIC")
ar_deaths <- select_ar_order(ts_deaths_id, max_p = 20, criterion = "BIC")

ar_cases$table
ar_deaths$table

cat("\nAR order selection (BIC):\n")
cat("Cases: best p =", ar_cases$best_p, "\n")
cat("Deaths: best p =", ar_deaths$best_p, "\n")

cat("\nCompact AR representation:\n")
cat("Cases:", format_ar_equation(ar_cases$best_fit), "\n")
cat("Deaths:", format_ar_equation(ar_deaths$best_fit), "\n")

# Save IC curves for order selection
png(filename = file.path(plot_dir, "05_ar_order_selection_bic.png"),
  width = 1400, height = 900, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

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

par(mfrow = c(1, 1))
dev.off()

cat("Saved AR order-selection plot to:",
  file.path(plot_dir, "05_ar_order_selection_bic.png"), "\n")

### 10. Task (c): In-sample forecasting with 70/30 train/test split -----------
#
# Procedure:
#   1. Split each transformed stationary series into 70 % training / 30 % test.
#   2. Fit AR(best_p) — the BIC-selected order from task b — on the training set.
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

cat("\n--- Task (c): In-sample AR forecast MSFE (80/20 split, log1p scale) ---\n")
cat(sprintf("Cases     AR(%d) | train n=%d | test n=%d | MSFE = sqrt(MSE) = %.6f\n",
            ar_cases$best_p,     fc_cases$n_train,     fc_cases$n_test,     fc_cases$msfe))
cat(sprintf("Deaths    AR(%d) | train n=%d | test n=%d | MSFE = sqrt(MSE) = %.6f\n",
            ar_deaths$best_p,    fc_deaths$n_train,    fc_deaths$n_test,    fc_deaths$msfe))

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

# Moving-average comparison on the test window (actual vs forecast)
calc_ma_metrics <- function(y_actual, y_forecast, k = 7) {
  w <- rep(1 / k, k)
  ma_actual <- as.numeric(stats::filter(y_actual, w, sides = 1))
  ma_forecast <- as.numeric(stats::filter(y_forecast, w, sides = 1))
  keep <- is.finite(ma_actual) & is.finite(ma_forecast)

  ma_rmse <- if (any(keep)) {
    sqrt(mean((ma_actual[keep] - ma_forecast[keep])^2))
  } else {
    NA_real_
  }

  ma_corr <- if (sum(keep) > 1) {
    cor(ma_actual[keep], ma_forecast[keep])
  } else {
    NA_real_
  }

  list(
    ma_actual = ma_actual,
    ma_forecast = ma_forecast,
    ma_rmse = ma_rmse,
    ma_corr = ma_corr,
    ma_window = k
  )
}

plot_ma_compare_panel <- function(res, model_label, series_name, col_act, col_fc, ma_obj) {
  t_all  <- res$time_all
  t_test <- t_all[res$n_train + seq_len(res$n_test)]

  ylim <- range(c(res$yact, res$yhat, ma_obj$ma_actual, ma_obj$ma_forecast), na.rm = TRUE)

  plot(t_test, res$yact,
       type = "l", col = col_act, lwd = 0.8,
       ylim = ylim,
       main = sprintf(
         "%s - %s | Moving average (%d-day) RMSE=%.4f Corr=%.3f",
         series_name, model_label, ma_obj$ma_window, ma_obj$ma_rmse, ma_obj$ma_corr
       ),
       xlab = "Time index", ylab = "log1p(value)")

  lines(t_test, res$yhat, col = col_fc, lwd = 1.2, lty = 2)
  lines(t_test, ma_obj$ma_actual, col = adjustcolor(col_act, alpha.f = 0.95), lwd = 2.0)
  lines(t_test, ma_obj$ma_forecast, col = adjustcolor(col_fc, alpha.f = 0.95), lwd = 2.0, lty = 3)

  legend("topright",
      legend = c("Actual (test)", "Forecast", "Moving average (actual)", "Moving average (forecast)"),
         col = c(col_act, col_fc, adjustcolor(col_act, 0.95), adjustcolor(col_fc, 0.95)),
         lty = c(1, 2, 1, 3),
         lwd = c(1, 1.2, 2, 2),
         bty = "n", cex = 0.75)
}

# Screen display
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_forecast_panel(fc_cases,     ar_cases$best_p,     "Cases",     "steelblue", "orange")
plot_forecast_panel(fc_deaths,    ar_deaths$best_p,    "Deaths",    "firebrick", "purple")
par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "06_ar_insample_forecast.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_forecast_panel(fc_cases,     ar_cases$best_p,     "Cases",     "steelblue", "orange")
plot_forecast_panel(fc_deaths,    ar_deaths$best_p,    "Deaths",    "firebrick", "purple")
par(mfrow = c(1, 1))
dev.off()

cat("Saved forecast plot to:", file.path(plot_dir, "06_ar_insample_forecast.png"), "\n")

# Moving-average trend/cycle comparison for AR forecasts
ma_ar_cases <- calc_ma_metrics(fc_cases$yact, fc_cases$yhat, k = 7)
ma_ar_deaths <- calc_ma_metrics(fc_deaths$yact, fc_deaths$yhat, k = 7)

cat("\n--- Task (c_b): AR moving-average comparison (7-day window, test window) ---\n")
cat(sprintf("Cases     AR(%d) | Moving-average RMSE = %.6f | Moving-average corr = %.4f\n",
      ar_cases$best_p, ma_ar_cases$ma_rmse, ma_ar_cases$ma_corr))
cat(sprintf("Deaths    AR(%d) | Moving-average RMSE = %.6f | Moving-average corr = %.4f\n",
      ar_deaths$best_p, ma_ar_deaths$ma_rmse, ma_ar_deaths$ma_corr))

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_ma_compare_panel(fc_cases, paste0("AR(", ar_cases$best_p, ")"), "Cases", "steelblue", "orange", ma_ar_cases)
plot_ma_compare_panel(fc_deaths, paste0("AR(", ar_deaths$best_p, ")"), "Deaths", "firebrick", "purple", ma_ar_deaths)
par(mfrow = c(1, 1))

png(filename = file.path(plot_dir, "06_b_ar_moving_average_comparison.png"),
  width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_ma_compare_panel(fc_cases, paste0("AR(", ar_cases$best_p, ")"), "Cases", "steelblue", "orange", ma_ar_cases)
plot_ma_compare_panel(fc_deaths, paste0("AR(", ar_deaths$best_p, ")"), "Deaths", "firebrick", "purple", ma_ar_deaths)
par(mfrow = c(1, 1))
dev.off()

cat("Saved MA comparison plot to:", file.path(plot_dir, "06_b_ar_moving_average_comparison.png"), "\n")

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

# 11.2b Load Germany daily vaccination data (source: RKI GitHub)
# Columns: Impfdatum, BundeslandId_Impfort, Impfstoff, Impfserie, Anzahl
vacc_url <- "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/main/Deutschland_Bundeslaender_COVID-19-Impfungen.csv"
vacc_file_name <- "Deutschland_Bundeslaender_COVID-19-Impfungen.csv"
vacc_file_path <- file.path(getwd(), vacc_file_name)

vacc_download_status <- download.file(vacc_url, destfile = vacc_file_path, mode = "wb")
if (vacc_download_status != 0) {
  stop("Vaccination download failed. Check internet connection or URL.")
}

vacc_raw <- read.csv(vacc_file_path, stringsAsFactors = FALSE)
vacc_raw$Impfdatum <- as.Date(vacc_raw$Impfdatum)

# Aggregate all doses nationally by date (all series, all states, all vaccines)
daily_vacc_raw <- vacc_raw %>%
  group_by(Meldedatum = Impfdatum) %>%
  summarise(vacc_daily = sum(Anzahl, na.rm = TRUE), .groups = "drop") %>%
  arrange(Meldedatum)

# Expand to full analysis date range; pre-vaccination dates get 0
# cum_vacc: running total of doses in millions (scaled to avoid numeric issues)
daily_vacc <- data.frame(Meldedatum = all_dates$Meldedatum) %>%
  left_join(daily_vacc_raw, by = "Meldedatum") %>%
  mutate(
    vacc_daily = replace(vacc_daily, is.na(vacc_daily), 0L),
    cum_vacc   = cumsum(vacc_daily) / 1e6
  )

# 11.3 Build the final daily predictor table
predictors_daily <- data.frame(Meldedatum = all_dates$Meldedatum) %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  left_join(daily_age_cases %>% select(Meldedatum, share_cases_60plus),
            by = "Meldedatum") %>%
  left_join(daily_age_deaths %>% select(Meldedatum, share_deaths_80plus),
            by = "Meldedatum") %>%
  left_join(daily_vacc %>% select(Meldedatum, cum_vacc), by = "Meldedatum") %>%
  mutate(
    share_cases_60plus  = replace(share_cases_60plus,  is.na(share_cases_60plus),  0),
    share_deaths_80plus = replace(share_deaths_80plus, is.na(share_deaths_80plus), 0),
    cum_vacc            = replace(cum_vacc,            is.na(cum_vacc),            0),
    weekday   = weekdays(Meldedatum),
    is_weekend = as.integer(weekday %in% c("Saturday", "Sunday")),
    time_idx   = as.numeric(Meldedatum - min(Meldedatum)) + 1
  )

cat("\nTask (d) predictors summary:\n")
cat("Temperature source: Open-Meteo Archive API\n")
cat("Vaccination source: RKI COVID-19-Impfungen_in_Deutschland\n")
cat("Mean temperature (C):", round(mean(predictors_daily$temp_c, na.rm = TRUE), 2), "\n")
cat("Mean share cases age 60+:", round(mean(predictors_daily$share_cases_60plus), 4), "\n")
cat("Mean share deaths age 80+:", round(mean(predictors_daily$share_deaths_80plus), 4), "\n")
cat("Max cumulative vaccinations (millions):", round(max(predictors_daily$cum_vacc), 2), "\n")

# 11.4 Align exogenous variables with log-scale original series
# Since we now use log1p(y) with d=1 (not pre-differenced), xreg should
# match the original series length n directly (Arima handles differencing).

if (nrow(predictors_daily) != length(ts_cases)) {
  stop("Predictor table length mismatch with original series length.")
}

build_xreg <- function(cols) {
  as.matrix(predictors_daily[, cols, drop = FALSE])
}

xreg_cases     <- build_xreg(c("temp_c", "cum_vacc", "is_weekend", "time_idx", "share_cases_60plus"))
xreg_deaths    <- build_xreg(c("temp_c", "cum_vacc", "is_weekend", "time_idx", "share_deaths_80plus"))
xreg_recovered <- build_xreg(c("temp_c", "cum_vacc", "is_weekend", "time_idx", "share_cases_60plus"))

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

cat("\n--- Task (d): ARX forecast MSFE (log1p scale) ---\n")
cat(sprintf("Cases     ARX(%d) | MSFE = %.6f\n", ar_cases$best_p, arx_cases$msfe))
cat(sprintf("Deaths    ARX(%d) | MSFE = %.6f\n", ar_deaths$best_p, arx_deaths$msfe))

cat("\n--- Task (d): AR vs ARX (MSFE on log1p scale) ---\n")
cat(sprintf("Cases:     AR = %.6f | ARX = %.6f\n", fc_cases$msfe, arx_cases$msfe))
cat(sprintf("Deaths:    AR = %.6f | ARX = %.6f\n", fc_deaths$msfe, arx_deaths$msfe))

cat("\nTask (d) selected ARX coefficients:\n")
cat("Cases coefficients:\n")
print(round(coef(arx_cases$fit), 4))
cat("Deaths coefficients:\n")
print(round(coef(arx_deaths$fit), 4))

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
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_arx_panel(arx_cases, ar_cases$best_p, "Cases", "steelblue", "orange")
plot_arx_panel(arx_deaths, ar_deaths$best_p, "Deaths", "firebrick", "purple")
par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "07_arx_forecast_with_predictors.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_arx_panel(arx_cases, ar_cases$best_p, "Cases", "steelblue", "orange")
plot_arx_panel(arx_deaths, ar_deaths$best_p, "Deaths", "firebrick", "purple")
par(mfrow = c(1, 1))
dev.off()

cat("Saved ARX forecast plot to:",
    file.path(plot_dir, "07_arx_forecast_with_predictors.png"), "\n")

# Moving-average trend/cycle comparison for ARX forecasts
ma_arx_cases <- calc_ma_metrics(arx_cases$yact, arx_cases$yhat, k = 7)
ma_arx_deaths <- calc_ma_metrics(arx_deaths$yact, arx_deaths$yhat, k = 7)

cat("\n--- Task (d_b): ARX moving-average comparison (7-day window, test window) ---\n")
cat(sprintf("Cases     ARX(%d) | Moving-average RMSE = %.6f | Moving-average corr = %.4f\n",
      ar_cases$best_p, ma_arx_cases$ma_rmse, ma_arx_cases$ma_corr))
cat(sprintf("Deaths    ARX(%d) | Moving-average RMSE = %.6f | Moving-average corr = %.4f\n",
      ar_deaths$best_p, ma_arx_deaths$ma_rmse, ma_arx_deaths$ma_corr))

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_ma_compare_panel(arx_cases, paste0("ARX(", ar_cases$best_p, ")"), "Cases", "steelblue", "orange", ma_arx_cases)
plot_ma_compare_panel(arx_deaths, paste0("ARX(", ar_deaths$best_p, ")"), "Deaths", "firebrick", "purple", ma_arx_deaths)
par(mfrow = c(1, 1))

png(filename = file.path(plot_dir, "07_b_arx_moving_average_comparison.png"),
  width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_ma_compare_panel(arx_cases, paste0("ARX(", ar_cases$best_p, ")"), "Cases", "steelblue", "orange", ma_arx_cases)
plot_ma_compare_panel(arx_deaths, paste0("ARX(", ar_deaths$best_p, ")"), "Deaths", "firebrick", "purple", ma_arx_deaths)
par(mfrow = c(1, 1))
dev.off()

cat("Saved MA comparison plot to:", file.path(plot_dir, "07_b_arx_moving_average_comparison.png"), "\n")


### 12. Task (e): Seasonal AR model  Phi(B^365) phi(B) y_t = epsilon_t ----------
#
# Here y_t is the ORIGINAL daily series (log1p-transformed for variance
# stabilisation). Stationarity and annual seasonality are handled entirely
# inside Arima() via d=1 (first difference) and D=1, period=365 (annual
# seasonal difference), so the model is SARIMA(p,1,0)(P,1,0)[365] on the
# log scale. This is equivalent to fitting Phi(B^365) phi(B) on the
# doubly-differenced series but works directly on the original data and
# avoids mixing the lag-7 weekly pre-differencing (used in tasks b–d) with
# an annual seasonal AR structure.


# 12.1 Seasonal AR order selection via auto.arima
# Constrain q = Q = 0 so the search stays in the SARIMA(p,1,0)(P,1,0)[365] family.
select_sar_order <- function(y, max_p = 5, max_P = 2, season = 365, ic = "bic") {
  fit <- forecast::auto.arima(
    y,
    d = 1,
    D = 1,
    seasonal = TRUE,
    max.p = max_p,
    max.q = 0,
    max.P = max_P,
    max.Q = 0,
    ic = ic,
    stepwise = TRUE,
    approximation = TRUE,
    allowmean = FALSE,
    allowdrift = FALSE
  )

  ord <- forecast::arimaorder(fit)

  list(
    best_p = unname(ord["p"]),
    best_P = unname(ord["P"]),
    best_bic = BIC(fit),
    best_fit = fit
  )
}

# Optional diagnostic: capture auto.arima trace output (all tried models) to file.
# This does NOT change model selection; it only records the search log.
save_autoarima_trace <- function(y, out_file, max_p = 5, max_P = 2, season = 365, ic = "bic") {
  trace_lines <- capture.output(
    forecast::auto.arima(
      y,
      d = 1,
      D = 1,
      seasonal = TRUE,
      max.p = max_p,
      max.q = 0,
      max.P = max_P,
      max.Q = 0,
      ic = ic,
      stepwise = TRUE,
      approximation = TRUE,
      allowmean = FALSE,
      allowdrift = FALSE,
      trace = TRUE
    )
  )
  writeLines(trace_lines, out_file)
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

# 12.3 Fit + forecast function (80/20 split, same as tasks c & d)
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

cat("\n=== Task (e): Seasonal AR – order selection via constrained auto.arima ===\n")
cat("(Model family: SARIMA(p,1,0)(P,1,0)[365] on log1p(original); q=Q=0 fixed)\n")

sar_ord_cases     <- select_sar_order(log_cases,     max_p = 21, max_P = 2, season = 365)
sar_ord_deaths    <- select_sar_order(log_deaths,    max_p = 21, max_P = 2, season = 365)

save_autoarima_trace(
  log_cases,
  out_file = file.path(plot_dir, "09_autoarima_cases_trace_bic.txt"),
  max_p = 21,
  max_P = 2,
  season = 365,
  ic = "bic"
)
save_autoarima_trace(
  log_deaths,
  out_file = file.path(plot_dir, "09_autoarima_deaths_trace_bic.txt"),
  max_p = 21,
  max_P = 2,
  season = 365,
  ic = "bic"
)

cat("Saved auto.arima trace logs to:\n")
cat(" -", file.path(plot_dir, "09_autoarima_cases_trace_bic.txt"), "\n")
cat(" -", file.path(plot_dir, "09_autoarima_deaths_trace_bic.txt"), "\n")

cat(sprintf("\nCases:     best p=%d, P=%d  (BIC=%.2f)\n",
            sar_ord_cases$best_p,     sar_ord_cases$best_P,     sar_ord_cases$best_bic))
cat(sprintf("Deaths:    best p=%d, P=%d  (BIC=%.2f)\n",
            sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    sar_ord_deaths$best_bic))

# 12.4 Print selected seasonal AR models
cat("\nSelected seasonal AR model – Cases:\n");     print(sar_ord_cases$best_fit)
cat("\nSelected seasonal AR model – Deaths:\n");    print(sar_ord_deaths$best_fit)

# 12.5 Fit on full series and print equations
fit_sar_cases <- sar_ord_cases$best_fit
fit_sar_deaths <- sar_ord_deaths$best_fit

format_sar_equation(fit_sar_cases,     "Cases",     season = 365)
format_sar_equation(fit_sar_deaths,    "Deaths",    season = 365)

# 12.6 70/30 forecast & MSFE (on log1p scale)
sar_cases     <- forecast_sar(log_cases,     sar_ord_cases$best_p,     sar_ord_cases$best_P)
sar_deaths    <- forecast_sar(log_deaths,    sar_ord_deaths$best_p,    sar_ord_deaths$best_P)

cat("\n--- Task (e): Seasonal AR MSFE (log1p scale, test portion) ---\n")
cat(sprintf("Cases     SAR(%d,%d)_365 | MSFE = %.6f\n",
            sar_ord_cases$best_p,     sar_ord_cases$best_P,     sar_cases$msfe))
cat(sprintf("Deaths    SAR(%d,%d)_365 | MSFE = %.6f\n",
            sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    sar_deaths$msfe))

# All three models (AR, ARX, SAR) now use log1p scale with d=1 internal differencing,
# so MSFE values are directly comparable across all models.
if (!exists("arx_cases") || !exists("arx_deaths")) {
  stop(
    "ARX results not found (arx_cases/arx_deaths). ",
    "Run Task (d) first, then run Task (e) comparison/plots."
  )
}

cat("\n--- Model comparison: AR vs ARX vs SAR (MSFE all on log1p scale) ---\n")
cat(sprintf("Cases:     AR=%.6f | ARX=%.6f | SAR=%.6f\n",
            fc_cases$msfe,     arx_cases$msfe,     sar_cases$msfe))
cat(sprintf("Deaths:    AR=%.6f | ARX=%.6f | SAR=%.6f\n",
            fc_deaths$msfe,    arx_deaths$msfe,    sar_deaths$msfe))

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
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_sar_panel(sar_cases,     sar_ord_cases$best_p,     sar_ord_cases$best_P,     "Cases",     "steelblue", "orange")
plot_sar_panel(sar_deaths,    sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    "Deaths",    "firebrick", "purple")
par(mfrow = c(1, 1))

# Save to PNG
png(filename = file.path(plot_dir, "08_sar_forecast.png"),
    width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_sar_panel(sar_cases,     sar_ord_cases$best_p,     sar_ord_cases$best_P,     "Cases",     "steelblue", "orange")
plot_sar_panel(sar_deaths,    sar_ord_deaths$best_p,    sar_ord_deaths$best_P,    "Deaths",    "firebrick", "purple")
par(mfrow = c(1, 1))
dev.off()
cat("Saved seasonal AR forecast plot to:", file.path(plot_dir, "08_sar_forecast.png"), "\n")

# Moving-average trend/cycle comparison for SAR forecasts
ma_sar_cases <- calc_ma_metrics(sar_cases$yact, sar_cases$yhat, k = 7)
ma_sar_deaths <- calc_ma_metrics(sar_deaths$yact, sar_deaths$yhat, k = 7)

cat("\n--- Task (e_b): SAR moving-average comparison (7-day window, test window) ---\n")
cat(sprintf("Cases     SAR(%d,%d)_365 | Moving-average RMSE = %.6f | Moving-average corr = %.4f\n",
      sar_ord_cases$best_p, sar_ord_cases$best_P, ma_sar_cases$ma_rmse, ma_sar_cases$ma_corr))
cat(sprintf("Deaths    SAR(%d,%d)_365 | Moving-average RMSE = %.6f | Moving-average corr = %.4f\n",
      sar_ord_deaths$best_p, sar_ord_deaths$best_P, ma_sar_deaths$ma_rmse, ma_sar_deaths$ma_corr))

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_ma_compare_panel(sar_cases,
            paste0("SAR(", sar_ord_cases$best_p, ",", sar_ord_cases$best_P, ")_365"),
            "Cases", "steelblue", "orange", ma_sar_cases)
plot_ma_compare_panel(sar_deaths,
            paste0("SAR(", sar_ord_deaths$best_p, ",", sar_ord_deaths$best_P, ")_365"),
            "Deaths", "firebrick", "purple", ma_sar_deaths)
par(mfrow = c(1, 1))

png(filename = file.path(plot_dir, "08_b_sar_moving_average_comparison.png"),
  width = 1600, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot_ma_compare_panel(sar_cases,
            paste0("SAR(", sar_ord_cases$best_p, ",", sar_ord_cases$best_P, ")_365"),
            "Cases", "steelblue", "orange", ma_sar_cases)
plot_ma_compare_panel(sar_deaths,
            paste0("SAR(", sar_ord_deaths$best_p, ",", sar_ord_deaths$best_P, ")_365"),
            "Deaths", "firebrick", "purple", ma_sar_deaths)
par(mfrow = c(1, 1))
dev.off()

cat("Saved MA comparison plot to:", file.path(plot_dir, "08_b_sar_moving_average_comparison.png"), "\n")

cat("\n--- Moving-average metric comparison (lower RMSE and higher corr are better) ---\n")
cat(sprintf("Cases:  AR RMSE=%.6f corr=%.4f | ARX RMSE=%.6f corr=%.4f | SAR RMSE=%.6f corr=%.4f\n",
      ma_ar_cases$ma_rmse, ma_ar_cases$ma_corr,
      ma_arx_cases$ma_rmse, ma_arx_cases$ma_corr,
      ma_sar_cases$ma_rmse, ma_sar_cases$ma_corr))
cat(sprintf("Deaths: AR RMSE=%.6f corr=%.4f | ARX RMSE=%.6f corr=%.4f | SAR RMSE=%.6f corr=%.4f\n",
      ma_ar_deaths$ma_rmse, ma_ar_deaths$ma_corr,
      ma_arx_deaths$ma_rmse, ma_arx_deaths$ma_corr,
      ma_sar_deaths$ma_rmse, ma_sar_deaths$ma_corr))

### --------------------------------------------------------------------------
### INTERPRETATION NOTES (for the report):
###
### For descriptive context, show raw level time-series plots.
### For model fitting (tasks c, d, e), all models use log1p(original series)
### with internal differencing via Arima(): AR and ARX use d=1; SAR uses
### d=1, D=1 (period=365). This provides a consistent, comparable framework.
###
### MSFE values across all three models (AR, ARX, SAR) are on the log1p
### scale and are therefore directly comparable without rescaling.
###
### Forecasting sections in tasks (c), (d), and (e) focus on Cases and Deaths.
### Recovered is kept only for exploratory/descriptive context above.
###
### Task (e): The seasonal AR model Phi(B^365) phi(B) y_t = epsilon_t
### (fit as SARIMA(p,1,0)(P,1,0)[365] on log1p scale) captures both
### short-run AR dynamics and annual seasonality. Compare MSFE of AR, ARX,
### and SAR to assess whether seasonal structure and/or external regressors
### improve forecast accuracy.
### --------------------------------------------------------------------------

ts_ex <-window(sar_cases$y_all, start = time(sar_cases$y_all)[sar_cases$n_train + 1])
ts_ex[[1]]
plot(ts_ex, type = "l")
lines(sar_cases$yhat, col = "red", lty = 2)

plot(ts_ex,type = 'l')
lines()

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


t_all  <- sar_cases$time_all
t_test <- t_all[sar_cases$n_train + seq_len(sar_cases$n_test)]
plot(t_test,sar_cases$yact, xlab = "Actual (test)", ylab = "SAR forecast",
     main = "SAR forecast vs actual (log1p scale)",lty = 2,type = "l")
lines(t_test, sar_cases$yhat, col = "red", lty = 2)





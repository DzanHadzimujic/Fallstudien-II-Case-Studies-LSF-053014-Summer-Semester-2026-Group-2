### ===========================================================================
### Case Studies for Project 1: Covid Forecasting with Linear Models
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
#   NeuerFall  = -1 : case is being REMOVED (correction) for exclude from totals
#   NeuerFall  =  0 : established case (in today's and yesterday's snapshot)
#   NeuerFall  =  1 : newly added case
# Keep NeuerFall in c(0, 1) for the case count series.
#
#   NeuerTodesfall = -9 : no death event in this group for exclude
#   NeuerTodesfall = -1 : death is being REMOVED (correction) for exclude
#   NeuerTodesfall =  0 : established death
#   NeuerTodesfall =  1 : newly reported death
# Keep NeuerTodesfall in c(0, 1) for the death count series.
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
# (NeuerTodesfall %in% c(0,1) for not -9 "no death" and not -1 "correction")
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

# Daily ts with frequency = 365 (approximate for ignores leap years but standard
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
detect_seasonal_period <- function(series, min_period = 2, max_period = 30) {
  spec_obj <- spec.pgram(log1p(series),
                         plot = FALSE,
                         demean = TRUE,
                         detrend = TRUE,
                         taper = 0)

  periods <- 1 / spec_obj$freq
  keep <- periods >= min_period & periods <= max_period

  if (!any(keep)) {
    return(7L)
  }

  peak_idx <- which.max(spec_obj$spec[keep])
  detected_period <- round(periods[keep][peak_idx])

  as.integer(max(2, detected_period))
}

season_cases <- detect_seasonal_period(ts_cases)+1
season_deaths <- detect_seasonal_period(ts_deaths)+1
season_recovered <- detect_seasonal_period(ts_recovered)+1

cat("\nDetected seasonal period (days) - cases:", 7, "\n")
cat("Detected seasonal period (days) - deaths:", 7, "\n")
cat("Detected seasonal period (days) - recovered:", 7, "\n")

# For ACF/PACF model identification, use transformed and differenced series:
# log1p stabilizes variance, seasonal difference uses detected period,
# first difference removes remaining trend.
ts_cases_id <- diff(diff(log1p(ts_cases), lag = 7), differences = 1)
ts_deaths_id <- diff(diff(log1p(ts_deaths), lag = 7), differences = 1)
ts_recovered_id <- diff(diff(log1p(ts_recovered), lag = 7), differences = 1)

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
### Recovered series is included, but note in the report that recovered counts
### are partly estimated by RKI and can be affected by reporting corrections.
###
### Task (e): The seasonal AR model Phi(B^365) phi(B) y_t = epsilon_t
### (fit as SARIMA(p,1,0)(P,1,0)[365] on log1p scale) captures both
### short-run AR dynamics and annual seasonality. Compare MSFE of AR, ARX,
### and SAR to assess whether seasonal structure and/or external regressors
### improve forecast accuracy.
### --------------------------------------------------------------------------


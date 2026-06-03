### ===========================================================================
### Case Studies – Project 2: Covid Forecasting with machine learning
### task(a): For each of the two methods you choose, create a one-step-ahead
### forecast ˆyt+1 based only on the
### lags of the cases series, making sure you re-estimate the model at each step;
### Try to see if there is a day of the week that may have relation  
### ===========================================================================

### 1. Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(rpart)
library(magrittr)
library(randomForest)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "Package 'jsonlite' is required for temperature download. ",
    "Install once with: install.packages('jsonlite')"
  )
}

if (!requireNamespace("zoo", quietly = TRUE)) {
  stop(
    "Package 'zoo' is required for filling missing temperature values. ",
    "Install once with: install.packages('zoo')"
  )
}


### 1.1 Import the data with URL
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

all_dates <- data.frame(
  Meldedatum = seq(min(daily_cases$Meldedatum), max(daily_cases$Meldedatum), by = "day")
)

daily_cases <- left_join(all_dates, daily_cases, by = "Meldedatum") %>%
  mutate(cases = replace(cases, is.na(cases), 0L))

### 4a. Periodogram of cases (dominant period detection) ---------------------
# Returns the top dominant periods (in days) from a log-transformed periodogram.
detect_top_periods <- function(series, min_period = 2, max_period = 600, n_peaks = 2) {
  series_ts <- ts(series, frequency = 365)
  spec_obj <- spec.pgram(
    log1p(series_ts),
    plot = FALSE,
    demean = TRUE,
    detrend = TRUE,
    taper = 0
  )

  periods <- 365 / spec_obj$freq
  keep <- periods >= min_period & periods <= max_period

  if (!any(keep)) {
    return(7L)
  }

  kept_spec <- spec_obj$spec[keep]
  kept_periods <- periods[keep]
  ranked <- order(kept_spec, decreasing = TRUE)
  ranked_periods <- unique(as.integer(round(kept_periods[ranked])))

  ranked_periods[seq_len(min(n_peaks, length(ranked_periods)))]
}

periodogram_obj <- spec.pgram(
  log1p(ts(daily_cases$cases, frequency = 365)),
  demean = TRUE,
  detrend = TRUE,
  taper = 0,
  plot = FALSE
)

periods_cases <- 365 / periodogram_obj$freq
keep_period_window <- periods_cases >= 2 & periods_cases <= 600
top_periods_cases <- detect_top_periods(daily_cases$cases, n_peaks = 2)
main_period_cases <- top_periods_cases[1]

# Confirm the exact spectral-bin point nearest to a weekly cycle.
weekly_target_days <- 7
weekly_idx <- which.min(abs(periods_cases - weekly_target_days))
weekly_period_nearest <- periods_cases[weekly_idx]
weekly_spec_nearest <- periodogram_obj$spec[weekly_idx]

# Additional peaks from the middle-left region of the periodogram.
mid_left_periods_cases <- detect_top_periods(
  daily_cases$cases,
  min_period = 2,
  max_period = 350,
  n_peaks = 2
)

y_upper <- if (any(keep_period_window, na.rm = TRUE)) {
  max(periodogram_obj$spec[keep_period_window], na.rm = TRUE)
} else {
  max(periodogram_obj$spec, na.rm = TRUE)
}

if (!is.finite(y_upper) || y_upper <= 0) {
  y_upper <- 1
}

cat("\nPeriodogram (cases) top periods in days:\n")
cat(sprintf("Primary period: %d days\n", main_period_cases))
if (length(top_periods_cases) > 1) {
  cat(sprintf("Secondary period: %d days\n", top_periods_cases[2]))
}
cat(sprintf(
  "Nearest point to 7-day cycle: %.2f days | spectral density: %.4f\n",
  weekly_period_nearest,
  weekly_spec_nearest
))
if (length(mid_left_periods_cases) > 0) {
  cat("Additional middle-left peaks:\n")
  for (p in mid_left_periods_cases) {
    cat(sprintf("  - %d days\n", p))
  }
}

periodogram_plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(periodogram_plot_dir)) {
  dir.create(periodogram_plot_dir, recursive = TRUE)
}

png(
  filename = file.path(periodogram_plot_dir, "second_assignment_periodogram_cases.png"),
  width = 1400,
  height = 900,
  res = 150
)
plot(
  periods_cases,
  periodogram_obj$spec,
  type = "p",
  pch = 16,
  cex = 0.8,
  col = "steelblue",
  xlim = c(2, 600),
  ylim = c(0, y_upper * 1.1),
  xlab = "Period (days)",
  ylab = "Spectral density",
  main = paste("Cases periodogram | top period:", main_period_cases, "days")
)
abline(v = top_periods_cases, col = c("red", "orange")[seq_along(top_periods_cases)], lty = 2, lwd = 1.5)
abline(v = mid_left_periods_cases,
  col = c("darkgreen", "purple")[seq_along(mid_left_periods_cases)],
  lty = 3,
  lwd = 1.5)
abline(v = weekly_period_nearest, col = "black", lty = 4, lwd = 1.4)
points(weekly_period_nearest, weekly_spec_nearest, pch = 8, cex = 1.2, col = "black")
grid(col = "gray80")
dev.off()

### 4b. External predictors: temperature and vaccinations ---------------------
temp_url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=51.1657&longitude=10.4515",
  "&start_date=", as.character(min(all_dates$Meldedatum)),
  "&end_date=", as.character(max(all_dates$Meldedatum)),
  "&daily=temperature_2m_mean",
  "&timezone=Europe%2FBerlin"
)

temp_json <- jsonlite::fromJSON(temp_url)

# Build a daily temperature series for Germany (country centroid) from Open-Meteo.
# `temp_c` is the daily mean 2-meter air temperature in degrees Celsius.
daily_temp <- data.frame(
  Meldedatum = as.Date(temp_json$daily$time),
  temp_c = as.numeric(temp_json$daily$temperature_2m_mean)
) %>%
  arrange(Meldedatum)

temp_missing_before <- sum(is.na(daily_temp$temp_c))
if (temp_missing_before > 0) {
  daily_temp <- daily_temp %>%
    mutate(
      # Fill occasional internal gaps by linear interpolation between neighboring days.
      temp_c = zoo::na.approx(temp_c, na.rm = FALSE),
      # Forward-fill remaining leading/trailing gaps using last observed value.
      temp_c = zoo::na.locf(temp_c, na.rm = FALSE),
      # Backward-fill any values still missing at the start of the series.
      temp_c = zoo::na.locf(temp_c, fromLast = TRUE, na.rm = FALSE)
    )
}
temp_missing_after <- sum(is.na(daily_temp$temp_c))
cat(sprintf("Temperature missing values: before = %d, after = %d\n", temp_missing_before, temp_missing_after))

vacc_url <- "https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/main/Deutschland_Bundeslaender_COVID-19-Impfungen.csv"
vacc_file_name <- "Deutschland_Bundeslaender_COVID-19-Impfungen.csv"
vacc_file_path <- file.path(getwd(), vacc_file_name)

vacc_download_status <- download.file(vacc_url, destfile = vacc_file_path, mode = "wb")
if (vacc_download_status != 0) {
  stop("Vaccination download failed. Check internet connection or URL.")
}

vacc_raw <- read.csv(vacc_file_path, stringsAsFactors = FALSE)
vacc_raw$Impfdatum <- as.Date(vacc_raw$Impfdatum)

daily_vacc_raw <- vacc_raw %>%
  group_by(Meldedatum = Impfdatum) %>%
  summarise(vacc_daily = sum(Anzahl, na.rm = TRUE), .groups = "drop") %>%
  arrange(Meldedatum)

daily_vacc <- all_dates %>%
  left_join(daily_vacc_raw, by = "Meldedatum") %>%
  mutate(
    vacc_daily = replace(vacc_daily, is.na(vacc_daily), 0),
    cum_vacc = cumsum(vacc_daily) / 1e6
  )

external_features <- all_dates %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  left_join(daily_vacc %>% select(Meldedatum, cum_vacc), by = "Meldedatum") %>%
  arrange(Meldedatum)

model_input <- daily_cases %>%
  left_join(external_features, by = "Meldedatum")

cat("\nExternal predictors summary:\n")
cat("Mean temperature (C):", round(mean(model_input$temp_c, na.rm = TRUE), 2), "\n")
cat("Max cumulative vaccinations (millions):", round(max(model_input$cum_vacc, na.rm = TRUE), 2), "\n")

### 5. Create lag features ----------------------------------------------------
# Builds supervised-learning features from the time series.
# target_log is next-day log cases; lag_* columns are historical case values.
lag_values <- c(1, 3, 6, 7, 10)

create_lag_features <- function(df, lags = lag_values) {
  out <- df %>%
    arrange(Meldedatum) %>%
    mutate(
      cases_log = log1p(cases),
      target_log = dplyr::lead(cases_log, 1),
      day_of_week = factor(
        as.integer(format(Meldedatum, "%u")),
        levels = 1:7,
        labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      ),
      month = factor(as.integer(format(Meldedatum, "%m")), levels = 1:12),
      trend_index = row_number(),
      is_weekend = factor(ifelse(day_of_week %in% c("Sat", "Sun"), "yes", "no"), levels = c("no", "yes"))
    )

  for (lag_value in lags) {
    lag_name <- paste0("lag_", lag_value)
    out[[lag_name]] <- dplyr::lag(out$cases_log, lag_value)
  }

  out
}

model_data <- create_lag_features(model_input)
lag_columns <- grep("^lag_", names(model_data), value = TRUE)
required_columns <- c("target_log", "temp_c", "cum_vacc", "day_of_week", "month", "trend_index", "is_weekend", lag_columns)
model_data <- model_data[complete.cases(model_data[, required_columns]), ]

cat("\nModeling rows after lag creation:", nrow(model_data), "\n")

### 6. Split data by time -----------------------------------------------------
temp_lags <- lag_values
vacc_lags <- lag_values

for (lag_value in temp_lags) {
  lag_name <- paste0("temp_c_lag", lag_value)
  model_data[[lag_name]] <- dplyr::lag(model_data$temp_c, lag_value)
}

for (lag_value in vacc_lags) {
  lag_name <- paste0("cum_vacc_lag", lag_value)
  model_data[[lag_name]] <- dplyr::lag(model_data$cum_vacc, lag_value)
}

covariate_lag_columns <- c(
  paste0("temp_c_lag", temp_lags),
  paste0("cum_vacc_lag", vacc_lags)
)

required_columns <- c("target_log", lag_columns, covariate_lag_columns)
model_data <- model_data[complete.cases(model_data[, required_columns]), ]

n_obs <- nrow(model_data)
train_end <- floor(0.60 * n_obs)
valid_start <- train_end + 1
valid_end <- floor(0.80 * n_obs)
test_start <- valid_end + 1
test_end <- n_obs

train_data <- model_data[seq_len(train_end), ]
valid_data <- model_data[valid_start:valid_end, ]
test_data <- model_data[test_start:test_end, ]

cat("Train period:", as.character(min(train_data$Meldedatum)), "to", as.character(max(train_data$Meldedatum)), "\n")
cat("Validation period:", as.character(min(valid_data$Meldedatum)), "to", as.character(max(valid_data$Meldedatum)), "\n")
cat("Test period:", as.character(min(test_data$Meldedatum)), "to", as.character(max(test_data$Meldedatum)), "\n")

# Runtime controls: fast_mode keeps methodology unchanged but reduces tuning cost.
fast_mode <- TRUE
validation_stride <- if (fast_mode) 7 else 1
rf_ntree_tune <- if (fast_mode) 150 else 500
rf_ntree_forecast <- if (fast_mode) 300 else 500

cat(sprintf("Runtime settings -> fast_mode=%s, validation_stride=%d, rf_ntree_tune=%d, rf_ntree_forecast=%d\n",
            fast_mode, validation_stride, rf_ntree_tune, rf_ntree_forecast))

# Task (a): lag-only predictors.
predictors_task_a <- lag_columns
# Task (b): lag predictors + lagged covariates.
predictors_task_b <- c(lag_columns, covariate_lag_columns)

# Computes RMSE on the log1p scale used for model fitting.
evaluate_predictions <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  c(RMSE = sqrt(mean((actual - predicted)^2)))
}

# Unified prediction wrapper so tree and random forest share one interface.
predict_log_scale <- function(fit, new_data, model_type) {
  switch(
    model_type,
    tree = predict(fit, new_data, type = "vector"),
    rf = predict(fit, new_data),
    stop("Unknown model type")
  )
}

# Creates the model formula target_log ~ predictors.
build_model_formula <- function(predictors) {
  as.formula(paste("target_log ~", paste(predictors, collapse = " + ")))
}

# Fits either a regression tree or random forest using a common call signature.
fit_model <- function(train_df, predictors, model_type, params) {
  model_formula <- build_model_formula(predictors)
  if (model_type == "tree") {
    return(rpart::rpart(
      model_formula,
      data = train_df,
      method = "anova",
      control = rpart::rpart.control(
        cp = params$cp,
        minsplit = params$minsplit,
        maxdepth = params$maxdepth,
        xval = 0
      )
    ))
  }

  if (model_type == "rf") {
    return(randomForest::randomForest(
      model_formula,
      data = train_df,
      ntree = params$ntree,
      mtry = params$mtry,
      nodesize = params$nodesize,
      importance = FALSE
    ))
  }

  stop("Unknown model type")
}

# Re-estimate model at every step and produce one-step-ahead forecasts.
# Trains on all data up to t-1, predicts at t, and repeats across an index window.
rolling_one_step_forecast <- function(data, predictors, model_type, params, start_idx, end_idx, step = 1) {
  eval_idx <- seq(from = start_idx, to = end_idx, by = step)
  preds <- numeric(length(eval_idx))
  actual <- numeric(length(eval_idx))

  needed_cols <- c("Meldedatum", "target_log", predictors)

  for (k in seq_along(eval_idx)) {
    t_idx <- eval_idx[k]
    train_rows <- seq_len(t_idx - 1)
    train_df <- data[train_rows, needed_cols, drop = FALSE]
    new_df <- data[t_idx, needed_cols, drop = FALSE]

    fit <- fit_model(train_df, predictors, model_type, params)
    preds[k] <- as.numeric(predict_log_scale(fit, new_df, model_type))
    actual[k] <- as.numeric(new_df$target_log)
  }

  metrics <- evaluate_predictions(actual, preds)

  list(
    dates = data$Meldedatum[eval_idx],
    actual = actual,
    predicted = preds,
    RMSE = metrics[["RMSE"]]
  )
}

# Evaluates each hyperparameter row with rolling one-step validation,
# then returns a table sorted by RMSE (lower is better).
tune_with_rolling_validation <- function(data, predictors, model_type, grid_df, valid_start_idx, valid_end_idx, valid_step = 1) {
  results <- vector("list", nrow(grid_df))

  for (i in seq_len(nrow(grid_df))) {
    params <- as.list(grid_df[i, , drop = FALSE])
    fc <- rolling_one_step_forecast(
      data = data,
      predictors = predictors,
      model_type = model_type,
      params = params,
      start_idx = valid_start_idx,
      end_idx = valid_end_idx,
      step = valid_step
    )

    results[[i]] <- cbind(
      data.frame(model = model_type, stringsAsFactors = FALSE),
      grid_df[i, , drop = FALSE],
      data.frame(RMSE = fc$RMSE)
    )
  }

  bind_rows(results) %>% arrange(RMSE)
}

### 7. Tune models with rolling validation -----------------------------------
tree_grid <- expand.grid(
  cp = c(0.0005, 0.001, 0.005),
  minsplit = c(20, 50, 100),
  maxdepth = c(3, 5, 7, 10)
)

rf_mtry_candidates <- unique(pmax(1, c(
  floor(sqrt(length(predictors_task_b))),
  floor(length(predictors_task_b) / 3),
  floor(length(predictors_task_b) / 2)
)))
rf_grid <- expand.grid(
  ntree = rf_ntree_tune,
  mtry = rf_mtry_candidates,
  nodesize = c(1, 5, 10)
)
rf_grid <- rf_grid[order(rf_grid$mtry, rf_grid$nodesize), ]

tree_val_a <- tune_with_rolling_validation(model_data, predictors_task_a, "tree", tree_grid, valid_start, valid_end, valid_step = validation_stride)
tree_val_b <- tune_with_rolling_validation(model_data, predictors_task_b, "tree", tree_grid, valid_start, valid_end, valid_step = validation_stride)
rf_val_a <- tune_with_rolling_validation(model_data, predictors_task_a, "rf", rf_grid, valid_start, valid_end, valid_step = validation_stride)
rf_val_b <- tune_with_rolling_validation(model_data, predictors_task_b, "rf", rf_grid, valid_start, valid_end, valid_step = validation_stride)

best_tree_a <- tree_val_a[1, ]
best_tree_b <- tree_val_b[1, ]
best_rf_a <- rf_val_a[1, ]
best_rf_b <- rf_val_b[1, ]

best_rf_a$ntree <- rf_ntree_forecast
best_rf_b$ntree <- rf_ntree_forecast

cat("\nBest params - Task (a) lag-only:\n")
print(bind_rows(best_tree_a, best_rf_a))
cat("\nBest params - Task (b) lagged covariates:\n")
print(bind_rows(best_tree_b, best_rf_b))

task_a_validation_summary <- data.frame(
  task = "(a) lag-only",
  model = c("Decision Tree", "Random Forest"),
  RMSE = c(as.numeric(best_tree_a$RMSE), as.numeric(best_rf_a$RMSE))
) %>% arrange(RMSE)

task_b_validation_summary <- data.frame(
  task = "(b) lagged covariates",
  model = c("Decision Tree", "Random Forest"),
  RMSE = c(as.numeric(best_tree_b$RMSE), as.numeric(best_rf_b$RMSE))
) %>% arrange(RMSE)

# Backward-compatible aliases in case individual sections are run interactively.
validation_summary_a <- task_a_validation_summary
validation_summary_b <- task_b_validation_summary

cat("\nValidation summary - Task (a):\n")
print(task_a_validation_summary)
cat("\nValidation summary - Task (b):\n")
print(task_b_validation_summary)

### 8. One-step-ahead test forecasts with re-estimation ----------------------
fc_tree_a <- rolling_one_step_forecast(model_data, predictors_task_a, "tree", as.list(best_tree_a), test_start, test_end)
fc_rf_a <- rolling_one_step_forecast(model_data, predictors_task_a, "rf", as.list(best_rf_a), test_start, test_end)
fc_tree_b <- rolling_one_step_forecast(model_data, predictors_task_b, "tree", as.list(best_tree_b), test_start, test_end)
fc_rf_b <- rolling_one_step_forecast(model_data, predictors_task_b, "rf", as.list(best_rf_b), test_start, test_end)

task_a_test_summary <- data.frame(
  task = "(a) lag-only",
  model = c("Decision Tree", "Random Forest"),
  RMSE = c(fc_tree_a$RMSE, fc_rf_a$RMSE)
) %>% arrange(RMSE)

task_b_test_summary <- data.frame(
  task = "(b) lagged covariates",
  model = c("Decision Tree", "Random Forest"),
  RMSE = c(fc_tree_b$RMSE, fc_rf_b$RMSE)
) %>% arrange(RMSE)

cat("\nTask (a) one-step-ahead test results (re-estimated each step):\n")
print(task_a_test_summary)

cat("\nTask (b) one-step-ahead test results (re-estimated each step):\n")
print(task_b_test_summary)

### 9. Permutation feature importance ----------------------------------------
train_valid_data <- model_data[seq_len(valid_end), , drop = FALSE]
test_eval_data <- model_data[test_start:test_end, , drop = FALSE]

# Chooses parameter set and metadata for the winning model of each task.
get_best_model_spec <- function(task_summary, best_tree_row, best_rf_row, predictors) {
  best_model <- task_summary$model[1]
  if (best_model == "Decision Tree") {
    list(
      model_type = "tree",
      params = list(
        cp = as.numeric(best_tree_row$cp),
        minsplit = as.numeric(best_tree_row$minsplit),
        maxdepth = as.numeric(best_tree_row$maxdepth)
      ),
      predictors = predictors,
      model_name = "Decision Tree"
    )
  } else {
    list(
      model_type = "rf",
      params = list(
        ntree = as.numeric(best_rf_row$ntree),
        mtry = as.numeric(best_rf_row$mtry),
        nodesize = as.numeric(best_rf_row$nodesize)
      ),
      predictors = predictors,
      model_name = "Random Forest"
    )
  }
}

# Permutes one feature at a time and measures RMSE degradation as importance.
permutation_importance <- function(fit, data, predictors, model_type, n_repeats = 5) {
  model_cols <- c("target_log", predictors)
  eval_df <- data[, model_cols, drop = FALSE]

  baseline_pred <- predict_log_scale(fit, eval_df, model_type)
  baseline_rmse <- as.numeric(evaluate_predictions(eval_df$target_log, baseline_pred)[["RMSE"]])

  imp_rows <- lapply(predictors, function(feature_name) {
    rmse_values <- numeric(n_repeats)
    for (r in seq_len(n_repeats)) {
      perm_df <- eval_df
      perm_df[[feature_name]] <- perm_df[[feature_name]][sample(seq_len(nrow(perm_df)))]
      perm_pred <- predict_log_scale(fit, perm_df, model_type)
      rmse_values[r] <- as.numeric(evaluate_predictions(perm_df$target_log, perm_pred)[["RMSE"]])
    }

    data.frame(
      feature = feature_name,
      baseline_rmse = baseline_rmse,
      permuted_rmse = mean(rmse_values),
      importance = mean(rmse_values) - baseline_rmse
    )
  })

  bind_rows(imp_rows) %>% arrange(desc(importance))
}

best_spec_a <- get_best_model_spec(task_a_test_summary, best_tree_a, best_rf_a, predictors_task_a)
best_spec_b <- get_best_model_spec(task_b_test_summary, best_tree_b, best_rf_b, predictors_task_b)

fit_best_a <- fit_model(train_valid_data[, c("target_log", best_spec_a$predictors), drop = FALSE],
                        best_spec_a$predictors,
                        best_spec_a$model_type,
                        best_spec_a$params)
fit_best_b <- fit_model(train_valid_data[, c("target_log", best_spec_b$predictors), drop = FALSE],
                        best_spec_b$predictors,
                        best_spec_b$model_type,
                        best_spec_b$params)

importance_task_a <- permutation_importance(
  fit_best_a,
  test_eval_data,
  best_spec_a$predictors,
  best_spec_a$model_type,
  n_repeats = 5
)

importance_task_b <- permutation_importance(
  fit_best_b,
  test_eval_data,
  best_spec_b$predictors,
  best_spec_b$model_type,
  n_repeats = 5
)

cat("\nPermutation importance - Task (a) using best model:\n")
print(head(importance_task_a, 10))
cat("\nPermutation importance - Task (b) using best model:\n")
print(head(importance_task_b, 10))

### 10. Plots ----------------------------------------------------------------
plot_dir <- file.path(getwd(), "plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# Creates RMSE comparison bars and highlights the lower-RMSE model as best.
build_validation_plot <- function(summary_df, title_text) {
  best_model <- summary_df$model[which.min(summary_df$RMSE)]
  plot_data <- summary_df %>%
    mutate(model_label = ifelse(model == best_model, paste0(model, " (best)"), model))

  ggplot(plot_data, aes(x = model_label, y = RMSE)) +
  geom_col(width = 0.65, fill = "#D95F02") +
  geom_text(aes(label = sprintf("%.3f", RMSE)),
            vjust = -0.35,
            size = 3.5) +
  labs(
    title = title_text,
    subtitle = "Best model is tagged with (best)",
    x = NULL,
    y = "Error on log1p scale",
    fill = NULL
  ) +
  theme_minimal(base_size = 12)
}

validation_plot_a <- build_validation_plot(task_a_validation_summary, "Task (a) lag-only: validation performance")
validation_plot_b <- build_validation_plot(task_b_validation_summary, "Task (b) lagged covariates: validation performance")

test_plot_a <- build_validation_plot(task_a_test_summary, "Task (a) lag-only: one-step-ahead test performance")
test_plot_b <- build_validation_plot(task_b_test_summary, "Task (b) lagged covariates: one-step-ahead test performance")

best_model_a <- task_a_test_summary$model[1]
best_model_b <- task_b_test_summary$model[1]

plot_task_a_best <- if (best_model_a == "Decision Tree") fc_tree_a else fc_rf_a
plot_task_b_best <- if (task_b_test_summary$model[1] == "Decision Tree") fc_tree_b else fc_rf_b

prediction_plot_a <- data.frame(
  Meldedatum = plot_task_a_best$dates,
  Actual_log1p = plot_task_a_best$actual,
  Predicted_log1p = plot_task_a_best$predicted
)

prediction_plot <- data.frame(
  Meldedatum = plot_task_b_best$dates,
  Actual_log1p = plot_task_b_best$actual,
  Predicted_log1p = plot_task_b_best$predicted
)

prediction_figure_a <- ggplot(prediction_plot_a, aes(x = Meldedatum)) +
  geom_line(aes(y = Actual_log1p, colour = "Actual log1p"), size = 0.6) +
  geom_line(aes(y = Predicted_log1p, colour = "Predicted log1p"), size = 0.6) +
  scale_colour_manual(values = c("Actual log1p" = "steelblue", "Predicted log1p" = "firebrick")) +
  labs(
    title = paste0("Task (a): best model one-step-ahead predictions - ", best_model_a),
    x = "Date",
    y = "log(1 + daily cases)",
    colour = NULL
  ) +
  theme_minimal(base_size = 12)

prediction_figure <- ggplot(prediction_plot, aes(x = Meldedatum)) +
  geom_line(aes(y = Actual_log1p, colour = "Actual log1p"), size = 0.6) +
  geom_line(aes(y = Predicted_log1p, colour = "Predicted log1p"), size = 0.6) +
  scale_colour_manual(values = c("Actual log1p" = "steelblue", "Predicted log1p" = "firebrick")) +
  labs(
    title = paste0("Task (b): best model one-step-ahead predictions - ", best_model_b),
    x = "Date",
    y = "log(1 + daily cases)",
    colour = NULL
  ) +
  theme_minimal(base_size = 12)

importance_plot_a <- ggplot(importance_task_a, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = paste0("Task (a) feature importance - ", best_spec_a$model_name),
    x = NULL,
    y = "Increase in RMSE"
  ) +
  theme_minimal(base_size = 12)

importance_plot_b <- ggplot(importance_task_b, aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "#D7191C") +
  coord_flip() +
  labs(
    title = paste0("Task (b) feature importance - ", best_spec_b$model_name),
    x = NULL,
    y = "Increase in RMSE"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(plot_dir, "second_assignment_test_predictions_task_a.png"), prediction_figure_a, width = 10, height = 5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_test_predictions.png"), prediction_figure, width = 10, height = 5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_test_predictions_task_b.png"), prediction_figure, width = 10, height = 5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_validation_models_task_a.png"), validation_plot_a, width = 10, height = 5.5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_validation_models_task_b.png"), validation_plot_b, width = 10, height = 5.5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_test_models_task_a.png"), test_plot_a, width = 10, height = 5.5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_test_models_task_b.png"), test_plot_b, width = 10, height = 5.5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_permutation_importance_task_a.png"), importance_plot_a, width = 9, height = 5.5, dpi = 150)
ggsave(file.path(plot_dir, "second_assignment_permutation_importance_task_b.png"), importance_plot_b, width = 9, height = 5.5, dpi = 150)

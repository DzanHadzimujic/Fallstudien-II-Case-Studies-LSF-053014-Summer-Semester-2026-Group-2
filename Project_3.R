### ===========================================================================
### Case Studies - Project 3: Forecasting the Covid series
### Rolling-window estimation, forecast comparison, and Diebold-Mariano tests
### ===========================================================================

library(dplyr)
library(readr)
library(forecast)
library(rpart)
library(randomForest)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install once with: install.packages('jsonlite')")
}

if (!requireNamespace("zoo", quietly = TRUE)) {
  stop("Package 'zoo' is required. Install once with: install.packages('zoo')")
}

set.seed(12345)

### 1. Data import ------------------------------------------------------------
analysis_start <- as.Date("2020-04-01")
analysis_end <- as.Date("2026-03-31")
lag_max <- 10

base_dir <- getwd()
plot_dir <- file.path(base_dir, "plots")
table_dir <- file.path(base_dir, "tables")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
if (!dir.exists(table_dir)) dir.create(table_dir, recursive = TRUE)

url <- "https://media.githubusercontent.com/media/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_SarsCov2_Infektionen.csv"
file_name <- "Aktuell_Deutschland_SarsCov2_Infektionen.csv"
file_path <- file.path(base_dir, file_name)

if (!file.exists(file_path)) {
  options(timeout = 600)
  download_status <- download.file(url, destfile = file_path, mode = "wb")
  if (download_status != 0) {
    stop("Download failed. Check internet connection or URL.")
  }
}

covid_raw <- read_csv(
  file_path,
  col_types = cols(
    IdLandkreis = col_integer(),
    Altersgruppe = col_character(),
    Geschlecht = col_character(),
    Meldedatum = col_date(format = "%Y-%m-%d"),
    Refdatum = col_date(format = "%Y-%m-%d"),
    IstErkrankungsbeginn = col_integer(),
    NeuerFall = col_integer(),
    NeuerTodesfall = col_integer(),
    NeuGenesen = col_integer(),
    AnzahlFall = col_integer(),
    AnzahlTodesfall = col_integer(),
    AnzahlGenesen = col_integer()
  )
) %>%
  filter(Meldedatum >= analysis_start, Meldedatum <= analysis_end)

covid_clean <- covid_raw %>%
  filter(NeuerFall %in% c(0L, 1L))

new_case_rows <- sum(covid_raw$NeuerFall == 1L, na.rm = TRUE)
cat("Row count with NeuerFall == 1 (new cases):", new_case_rows, "\n")

daily_cases <- covid_clean %>%
  group_by(Meldedatum) %>%
  summarise(cases = sum(AnzahlFall, na.rm = TRUE), .groups = "drop") %>%
  arrange(Meldedatum)

all_dates <- data.frame(
  Meldedatum = seq(min(daily_cases$Meldedatum), max(daily_cases$Meldedatum), by = "day")
)

daily_cases <- left_join(all_dates, daily_cases, by = "Meldedatum") %>%
  mutate(cases = replace(cases, is.na(cases), 0L))

# Basic series plots for report context: original level and log1p scale.
png(filename = file.path(plot_dir, "project3_cases_timeseries.png"), width = 1400, height = 800, res = 150)
plot(
  daily_cases$Meldedatum,
  daily_cases$cases,
  type = "l",
  col = "steelblue",
  lwd = 1,
  xlab = "Date",
  ylab = "Daily cases",
  main = "Germany daily COVID-19 cases"
)
dev.off()

png(filename = file.path(plot_dir, "project3_cases_timeseries_log1p.png"), width = 1400, height = 800, res = 150)
plot(
  daily_cases$Meldedatum,
  log1p(daily_cases$cases),
  type = "l",
  col = "steelblue",
  lwd = 1,
  xlab = "Date",
  ylab = "log(1 + daily cases)",
  main = "Germany daily COVID-19 cases (log1p scale)"
)
dev.off()

daily_cases_log <- log1p(daily_cases$cases)
daily_cases_log_diff <- diff(diff(daily_cases_log, lag = 7), differences = 1)
daily_cases_log_diff_dates <- tail(daily_cases$Meldedatum, length(daily_cases_log_diff))

length(daily_cases_log_diff_dates)
length(daily_cases_log_diff)


png(filename = file.path(plot_dir, "project3_cases_log1p_diff.png"), width = 1400, height = 800, res = 150)
plot(
  daily_cases_log_diff_dates,
  daily_cases_log_diff,
  type = "l",
  col = "steelblue",
  lwd = 1,
  xlab = "Date",
  ylab = "First difference after 7-lag difference of log(1 + daily cases)",
  main = "Germany daily COVID-19 cases (first and 7-lag differences of log1p scale)"
)
abline(h = 0, lty = 2, col = "gray50")
dev.off()

png(filename = file.path(plot_dir, "project3_cases_log1p_diff_acf_pacf.png"), width = 1600, height = 800, res = 150)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
acf(
  daily_cases_log_diff,
  main = "ACF of 1st + 7-lag differenced log1p cases",
  lag.max = 40,
  na.action = na.pass
)
pacf(
  daily_cases_log_diff,
  main = "PACF of 1st + 7-lag differenced log1p cases",
  lag.max = 40,
  na.action = na.pass
)
dev.off()

### 2. Reused predictors from prior projects ---------------------------------
temp_state_locations <- data.frame(
  bundesland = c(
    "Baden-Wuerttemberg", "Bayern", "Berlin", "Brandenburg",
    "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
    "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland",
    "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thueringen"
  ),
  latitude = c(
    48.7758, 48.1374, 52.5200, 52.3906,
    53.0793, 53.5511, 50.0782, 53.6355,
    52.3759, 51.2277, 49.9929, 49.2402,
    51.0504, 52.1205, 54.3233, 50.9848
  ),
  longitude = c(
    9.1829, 11.5755, 13.4050, 13.0645,
    8.8017, 9.9937, 8.2398, 11.4012,
    9.7320, 6.7735, 8.2473, 6.9969,
    13.7373, 11.6276, 10.1228, 11.0299
  ),
  stringsAsFactors = FALSE
)

fetch_state_temperature <- function(state_name, latitude, longitude, start_date, end_date) {
  temp_url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", latitude,
    "&longitude=", longitude,
    "&start_date=", as.character(start_date),
    "&end_date=", as.character(end_date),
    "&daily=temperature_2m_mean",
    "&timezone=Europe%2FBerlin"
  )

  tryCatch(
    {
      temp_json <- jsonlite::fromJSON(temp_url)
      data.frame(
        Meldedatum = as.Date(temp_json$daily$time),
        temp_c = as.numeric(temp_json$daily$temperature_2m_mean),
        bundesland = state_name,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      warning(sprintf("Temperature download failed for %s: %s", state_name, e$message))
      data.frame(
        Meldedatum = all_dates$Meldedatum,
        temp_c = NA_real_,
        bundesland = state_name,
        stringsAsFactors = FALSE
      )
    }
  )
}

daily_temp_states <- bind_rows(lapply(seq_len(nrow(temp_state_locations)), function(i) {
  fetch_state_temperature(
    state_name = temp_state_locations$bundesland[i],
    latitude = temp_state_locations$latitude[i],
    longitude = temp_state_locations$longitude[i],
    start_date = min(all_dates$Meldedatum),
    end_date = max(all_dates$Meldedatum)
  )
}))

daily_temp <- daily_temp_states %>%
  group_by(Meldedatum) %>%
  summarise(
    temp_c = mean(temp_c, na.rm = TRUE),
    states_available = sum(!is.na(temp_c)),
    .groups = "drop"
  ) %>%
  mutate(temp_c = ifelse(states_available == 0, NA_real_, temp_c)) %>%
  arrange(Meldedatum)

daily_temp <- all_dates %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  mutate(states_available = replace(states_available, is.na(states_available), 0L)) %>%
  mutate(
    temp_c = zoo::na.approx(temp_c, na.rm = FALSE),
    temp_c = zoo::na.locf(temp_c, na.rm = FALSE),
    temp_c = zoo::na.locf(temp_c, fromLast = TRUE, na.rm = FALSE)
  )

top_age_group <- covid_clean %>%
  group_by(Altersgruppe) %>%
  summarise(total_cases = sum(AnzahlFall, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  slice(1) %>%
  pull(Altersgruppe)

daily_top_age <- covid_clean %>%
  mutate(top_age_cases = ifelse(Altersgruppe == top_age_group, AnzahlFall, 0L)) %>%
  group_by(Meldedatum) %>%
  summarise(top_age_cases = sum(top_age_cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(Meldedatum)

model_input <- all_dates %>%
  left_join(daily_cases, by = "Meldedatum") %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  left_join(daily_top_age, by = "Meldedatum") %>%
  mutate(
    temp_c = replace(temp_c, is.na(temp_c), 0),
    top_age_cases = replace(top_age_cases, is.na(top_age_cases), 0L),
    start_of_week = as.integer(format(Meldedatum, "%u") == 1),
    day_of_week = factor(
      as.integer(format(Meldedatum, "%u")),
      levels = 1:7,
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
  ) %>%
  arrange(Meldedatum)

### 3. Supervised learning frame ---------------------------------------------
make_supervised_data <- function(df, lag_max = 10) {
  out <- df %>%
    arrange(Meldedatum) %>%
    mutate(
      cases_log = log1p(cases),
      current_date = Meldedatum,
      target_cases = dplyr::lead(cases, 1),
      target_log = dplyr::lead(cases_log, 1),
      target_date = dplyr::lead(Meldedatum, 1)
    )

  # Build lagged log-case predictors up to lag_max for one-step-ahead forecasting.
  for (k in seq_len(lag_max)) {
    out[[paste0("lag_", k)]] <- dplyr::lag(out$cases_log, k)
  }

  required_cols <- c(
    "target_cases",
    "target_log",
    "target_date",
    "temp_c",
    "top_age_cases",
    "start_of_week",
    paste0("lag_", seq_len(lag_max))
  )

  out[complete.cases(out[, required_cols]), , drop = FALSE]
}

model_data <- make_supervised_data(model_input, lag_max = lag_max)
lag_cols <- paste0("lag_", seq_len(lag_max))

n_obs <- nrow(model_data)
train_end <- floor(0.8 * n_obs)
train_rows <- seq_len(train_end)
test_rows <- seq.int(train_end + 1, n_obs)

cat("Observations for supervised forecasting:", n_obs, "\n")
cat("Training rows:", length(train_rows), "| Test rows:", length(test_rows), "\n")
cat("Test period:", as.character(min(model_data$target_date[test_rows])), "to", as.character(max(model_data$target_date[test_rows])), "\n")

### 4. Helper functions -------------------------------------------------------
rmse <- function(actual, forecast) {
  sqrt(mean((as.numeric(actual) - as.numeric(forecast))^2))
}

to_cases <- function(x) {
  pmax(0, expm1(as.numeric(x)))
}

build_formula <- function(extra_terms) {
  rhs <- c(lag_cols, extra_terms)
  as.formula(paste("target_log ~", paste(rhs, collapse = " + ")))
}

build_covariate_only_formula <- function(extra_terms) {
  as.formula(paste("target_log ~", paste(extra_terms, collapse = " + ")))
}

fit_ols <- function(train_df, extra_terms) {
  lm(build_formula(extra_terms), data = train_df)
}

fit_ols_covariate_only <- function(train_df, extra_terms) {
  lm(build_covariate_only_formula(extra_terms), data = train_df)
}

fit_tree <- function(train_df, extra_terms, cp, minsplit, maxdepth) {
  rpart::rpart(
    build_formula(extra_terms),
    data = train_df,
    method = "anova",
    control = rpart::rpart.control(cp = cp, minsplit = minsplit, maxdepth = maxdepth, xval = 0)
  )
}

fit_rf <- function(train_df, extra_terms, ntree, mtry, nodesize) {
  randomForest::randomForest(
    build_formula(extra_terms),
    data = train_df,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    importance = FALSE
  )
}

predict_to_cases <- function(fit, new_data, model_type) {
  pred_log <- switch(
    model_type,
    lm = predict(fit, new_data),
    tree = predict(fit, new_data, type = "vector"),
    rf = predict(fit, new_data),
    stop("Unknown model type")
  )
  to_cases(pred_log)
}

rolling_supervised_forecast <- function(data, eval_rows, extra_terms, fit_fun, model_type, fit_args = list()) {
  preds <- numeric(length(eval_rows))
  actual <- numeric(length(eval_rows))

  keep_cols <- c("target_cases", "target_log", "target_date", "current_date", lag_cols, extra_terms)

  # Rolling/expanding re-estimation: at each row_id, fit on 1:(row_id-1), then predict row_id.
  for (i in seq_along(eval_rows)) {
    row_id <- eval_rows[i]
    train_df <- data[seq_len(row_id - 1), keep_cols, drop = FALSE]
    new_df <- data[row_id, keep_cols, drop = FALSE]

    # Refit the model at each step using only information available up to that date.
    fit <- do.call(fit_fun, c(list(train_df = train_df, extra_terms = extra_terms), fit_args))
    preds[i] <- predict_to_cases(fit, new_df, model_type)
    actual[i] <- new_df$target_cases
  }

  data.frame(
    target_date = data$target_date[eval_rows],
    actual = actual,
    forecast = preds,
    error = actual - preds,
    sq_error = (actual - preds)^2,
    stringsAsFactors = FALSE
  )
}

static_supervised_forecast <- function(data, train_rows, eval_rows, extra_terms, fit_fun, model_type, fit_args = list()) {
  keep_cols <- c("target_cases", "target_log", "target_date", "current_date", lag_cols, extra_terms)
  train_df <- data[train_rows, keep_cols, drop = FALSE]
  test_df <- data[eval_rows, keep_cols, drop = FALSE]

  # Task-c setup: fit once on initial training set, then forecast full test set.
  fit <- do.call(fit_fun, c(list(train_df = train_df, extra_terms = extra_terms), fit_args))
  preds <- predict_to_cases(fit, test_df, model_type)
  actual <- test_df$target_cases

  data.frame(
    target_date = data$target_date[eval_rows],
    actual = as.numeric(actual),
    forecast = as.numeric(preds),
    error = as.numeric(actual - preds),
    sq_error = as.numeric((actual - preds)^2),
    stringsAsFactors = FALSE
  )
}

rolling_ar_forecast <- function(full_cases, data, eval_rows, p, d = 0) {
  preds <- numeric(length(eval_rows))
  actual <- numeric(length(eval_rows))

  # ARIMA benchmark loop: refit ARIMA(p,d,0) each day and produce a one-step-ahead forecast.
  for (i in seq_along(eval_rows)) {
    row_id <- eval_rows[i]
    current_date <- data$current_date[row_id]

    # Restrict training to data available up to the forecast origin date.
    y_train <- log1p(full_cases$cases[full_cases$Meldedatum <= current_date])
    fit <- Arima(y_train, order = c(p, d, 0), include.mean = (d == 0), method = "ML")
    fc <- forecast(fit, h = 1)

    # Convert forecast back to original scale so evaluation is done on case counts.
    preds[i] <- to_cases(fc$mean)
    actual[i] <- data$target_cases[row_id]
  }

  data.frame(
    target_date = data$target_date[eval_rows],
    actual = actual,
    forecast = preds,
    error = actual - preds,
    sq_error = (actual - preds)^2,
    stringsAsFactors = FALSE
  )
}

rolling_sar_forecast <- function(full_cases, data, eval_rows, p, P, season = 7) {
  preds <- numeric(length(eval_rows))
  actual <- numeric(length(eval_rows))

  # Seasonal benchmark loop: same rolling refit pattern but with seasonal AR terms.
  for (i in seq_along(eval_rows)) {
    row_id <- eval_rows[i]
    current_date <- data$current_date[row_id]
    y_train <- log1p(full_cases$cases[full_cases$Meldedatum <= current_date])
    fit <- Arima(
      y_train,
      order = c(p, 1, 0),
      seasonal = list(order = c(P, 1, 0), period = season),
      include.mean = FALSE,
      method = "CSS-ML"
    )
    fc <- forecast(fit, h = 1)

    # Back-transform to original scale for RMSFE/DM comparability across all models.
    preds[i] <- to_cases(fc$mean)
    actual[i] <- data$target_cases[row_id]
  }

  data.frame(
    target_date = data$target_date[eval_rows],
    actual = actual,
    forecast = preds,
    error = actual - preds,
    sq_error = (actual - preds)^2,
    stringsAsFactors = FALSE
  )
}

select_sar_order <- function(series_log, train_end_row, max_p = 5, max_P = 2, season = 7) {
  best <- list(bic = Inf, p = 0, P = 0)

  # Grid-search over (p, P): keep the specification with the smallest BIC.
  for (p in 0:max_p) {
    for (P in 0:max_P) {
      fit <- tryCatch(
        Arima(
          series_log[seq_len(train_end_row)],
          order = c(p, 1, 0),
          seasonal = list(order = c(P, 1, 0), period = season),
          include.mean = FALSE,
          method = "CSS-ML"
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        bic <- BIC(fit)
        if (bic < best$bic) {
          best <- list(bic = bic, p = p, P = P, fit = fit)
        }
      }
    }
  }
  best
}

select_ar_order <- function(series_log, train_end_row, max_p = 10, max_d = 2) {
  best <- list(bic = Inf, p = 0, d = 0)

  # Grid-search over (p, d) with q fixed at 0.
  for (p in 0:max_p) {
    for (d in 0:max_d) {
      fit <- tryCatch(
        Arima(
          series_log[seq_len(train_end_row)],
          order = c(p, d, 0),
          include.mean = (d == 0),
          method = "ML"
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        bic <- BIC(fit)
        if (bic < best$bic) {
          best <- list(bic = bic, p = p, d = d, fit = fit)
        }
      }
    }
  }
  best
}

### 5. Baselines --------------------------------------------------------------
series_log <- model_data$cases_log
ar_choice <- list(best_p = 7)
ar_taska_choice <- select_ar_order(series_log, train_end_row = train_end, max_p = 10, max_d = 1)
sar_choice <- select_sar_order(series_log, train_end_row = train_end, max_p = 5, max_P = 2, season = 7)

cat("Fixed benchmark AR order: p =", ar_choice$best_p, ", d = 0, q = 0\n")
cat("Task-a selected ARIMA order by BIC: p =", ar_taska_choice$p, ", d =", ar_taska_choice$d, ", q = 0\n")
cat("Selected seasonal AR orders by BIC: p =", sar_choice$p, ", P =", sar_choice$P, "\n")

ar_roll <- rolling_ar_forecast(daily_cases, model_data, test_rows, ar_choice$best_p, 0) %>%
  mutate(model = paste0("AR(", ar_choice$best_p, ")_roll"))

ar_taska_roll <- rolling_ar_forecast(daily_cases, model_data, test_rows, ar_taska_choice$p, ar_taska_choice$d) %>%
  mutate(model = paste0("ARIMA(", ar_taska_choice$p, ",", ar_taska_choice$d, ",0)_taska"))

sar_roll <- rolling_sar_forecast(daily_cases, model_data, test_rows, sar_choice$p, sar_choice$P, season = 7) %>%
  mutate(model = paste0("SAR(", sar_choice$p, ",", sar_choice$P, ")[7]_roll"))

### 6. Linear models ----------------------------------------------------------
covariate_specs <- list(
  age = c("top_age_cases"),
  week = c("start_of_week"),
  temp = c("temp_c")
)

covariate_all <- c("top_age_cases", "start_of_week", "temp_c")

linear_static <- bind_rows(lapply(names(covariate_specs), function(spec_name) {
  static_supervised_forecast(
    data = model_data,
    train_rows = train_rows,
    eval_rows = test_rows,
    extra_terms = covariate_specs[[spec_name]],
    fit_fun = fit_ols_covariate_only,
    model_type = "lm"
  ) %>% mutate(model = paste0("LM_", spec_name, "_static"))
}))

linear_roll <- bind_rows(lapply(names(covariate_specs), function(spec_name) {
  rolling_supervised_forecast(
    data = model_data,
    eval_rows = test_rows,
    extra_terms = covariate_specs[[spec_name]],
    fit_fun = fit_ols_covariate_only,
    model_type = "lm"
  ) %>% mutate(model = paste0("LM_", spec_name, "_roll"))
}))

### 7. Machine learning models -----------------------------------------------
tree_grid <- expand.grid(
  cp = c(0.0005, 0.001, 0.005),
  minsplit = c(50, 100),
  maxdepth = c(3, 5, 7)
)

rf_grid <- expand.grid(
  ntree = c(200L),
  mtry = unique(pmax(1L, c(floor(sqrt(length(lag_cols))), floor(length(lag_cols) / 3), floor(length(lag_cols) / 2)))),
  nodesize = c(1L, 5L)
)

# Runtime switch for tuning speed: TRUE uses weekly validation steps, FALSE uses daily steps.
fast_mode <- TRUE
validation_stride <- if (fast_mode) 7 else 1
cat("Validation stride for tuning:", validation_stride, "(fast_mode =", fast_mode, ")\n")

tune_inner_set <- function(data, extra_terms, model_type, grid_df, valid_step = validation_stride) {
  inner_end <- floor(0.6 * train_end)
  validation_end <- floor(0.8 * train_end)
  inner_eval <- seq.int(inner_end + 1, validation_end, by = valid_step)
  training_data <- data[seq_len(train_end), seq_len(ncol(data)), drop = FALSE]
  results <- vector("list", nrow(grid_df))

  # Loop over each hyperparameter combination and score by rolling validation RMSFE.
  for (i in seq_len(nrow(grid_df))) {
    args <- as.list(grid_df[i, , drop = FALSE])
    preds <- if (model_type == "tree") {
      rolling_supervised_forecast(
        data = training_data,
        eval_rows = inner_eval,
        extra_terms = extra_terms,
        fit_fun = fit_tree,
        model_type = "tree",
        fit_args = args
      )
    } else {
      rolling_supervised_forecast(
        data = training_data,
        eval_rows = inner_eval,
        extra_terms = extra_terms,
        fit_fun = fit_rf,
        model_type = "rf",
        fit_args = args
      )
    }

    # Store one validation score per grid row so we can rank configurations.
    results[[i]] <- cbind(grid_df[i, , drop = FALSE], RMSFE = rmse(preds$actual, preds$forecast))
  }

  bind_rows(results) %>% arrange(RMSFE)
}

tree_best_lags <- tune_inner_set(model_data, character(0), "tree", tree_grid)
tree_best_cov <- tune_inner_set(model_data, covariate_all, "tree", tree_grid)
rf_best_lags <- tune_inner_set(model_data, character(0), "rf", rf_grid)
rf_best_cov <- tune_inner_set(model_data, covariate_all, "rf", rf_grid)

best_tree_lags <- tree_best_lags[1, ]
best_tree_cov <- tree_best_cov[1, ]
best_rf_lags <- rf_best_lags[1, ]
best_rf_cov <- rf_best_cov[1, ]

tree_lags_roll <- rolling_supervised_forecast(
  data = model_data,
  eval_rows = test_rows,
  extra_terms = character(0),
  fit_fun = fit_tree,
  model_type = "tree",
  fit_args = list(
    cp = as.numeric(best_tree_lags$cp),
    minsplit = as.numeric(best_tree_lags$minsplit),
    maxdepth = as.numeric(best_tree_lags$maxdepth)
  )
) %>% mutate(model = "Tree_lags_roll")

tree_cov_roll <- rolling_supervised_forecast(
  data = model_data,
  eval_rows = test_rows,
  extra_terms = covariate_all,
  fit_fun = fit_tree,
  model_type = "tree",
  fit_args = list(
    cp = as.numeric(best_tree_cov$cp),
    minsplit = as.numeric(best_tree_cov$minsplit),
    maxdepth = as.numeric(best_tree_cov$maxdepth)
  )
) %>% mutate(model = "Tree_cov_roll")

rf_lags_roll <- rolling_supervised_forecast(
  data = model_data,
  eval_rows = test_rows,
  extra_terms = character(0),
  fit_fun = fit_rf,
  model_type = "rf",
  fit_args = list(
    ntree = as.integer(best_rf_lags$ntree),
    mtry = as.integer(best_rf_lags$mtry),
    nodesize = as.integer(best_rf_lags$nodesize)
  )
) %>% mutate(model = "RF_lags_roll")

rf_cov_roll <- rolling_supervised_forecast(
  data = model_data,
  eval_rows = test_rows,
  extra_terms = covariate_all,
  fit_fun = fit_rf,
  model_type = "rf",
  fit_args = list(
    ntree = as.integer(best_rf_cov$ntree),
    mtry = as.integer(best_rf_cov$mtry),
    nodesize = as.integer(best_rf_cov$nodesize)
  )
) %>% mutate(model = "RF_cov_roll")

### 8. Forecast collection ----------------------------------------------------
forecast_table <- bind_rows(
  ar_roll,
  ar_taska_roll,
  sar_roll,
  linear_static,
  linear_roll,
  tree_lags_roll,
  tree_cov_roll,
  rf_lags_roll,
  rf_cov_roll
) %>%
  mutate(
    actual = as.numeric(actual),
    forecast = as.numeric(forecast),
    error = as.numeric(error),
    sq_error = as.numeric(sq_error)
  )

rmsfe_table <- forecast_table %>%
  group_by(model) %>%
  summarise(
    RMSFE = sqrt(mean(sq_error)),
    MAE = mean(abs(error)),
    .groups = "drop"
  ) %>%
  arrange(RMSFE)

print(rmsfe_table)
write.csv(forecast_table, file.path(table_dir, "project3_forecasts.csv"), row.names = FALSE)
write.csv(rmsfe_table, file.path(table_dir, "project3_rmsfe_table.csv"), row.names = FALSE)

# Build LaTeX tables for direct inclusion in the report.
build_latex_table <- function(df, caption_text, label_text, align, headers) {
  bs <- intToUtf8(92)
  row_break <- paste0(bs, bs)

  header_line <- paste(headers, collapse = " & ")
  body_lines <- apply(df, 1, function(row) {
    paste0(paste(row, collapse = " & "), " ", row_break)
  })

  lines <- c(
    paste0(bs, "begin{table}[ht]"),
    paste0(bs, "centering"),
    paste0(bs, "caption{", caption_text, "}"),
    paste0(bs, "label{", label_text, "}"),
    paste0(bs, "begin{tabular}{", align, "}"),
    paste0(bs, "hline"),
    paste0(header_line, " ", row_break),
    paste0(bs, "hline"),
    body_lines,
    paste0(bs, "hline"),
    paste0(bs, "end{tabular}"),
    paste0(bs, "end{table}")
  )

  paste(lines, collapse = "\n")
}

fmt_model <- function(x) {
  gsub("_", "\\\\_", as.character(x), fixed = TRUE)
}

selection_latex_df <- data.frame(
  model = c("ARIMA", "SARIMA"),
  order = c(
    paste0("(", ar_taska_choice$p, ",", ar_taska_choice$d, ",0)"),
    paste0("(", sar_choice$p, ",1,0) x (", sar_choice$P, ",1,0)_{7}")
  ),
  BIC = sprintf("%.4f", c(ar_taska_choice$bic, sar_choice$bic)),
  stringsAsFactors = FALSE
)

selection_tex <- build_latex_table(
  selection_latex_df,
  "BIC-selected ARIMA and SARIMA specifications",
  "tab:project3_bic_selection",
  "lcc",
  c("Model", "Selected order", "BIC")
)

writeLines(selection_tex, file.path(table_dir, "project3_bic_selection.tex"))

rmsfe_latex_df <- rmsfe_table %>%
  mutate(
    model = fmt_model(model),
    RMSFE = sprintf("%.4f", RMSFE),
    MAE = sprintf("%.4f", MAE)
  ) %>%
  as.data.frame(stringsAsFactors = FALSE)

rmsfe_tex <- build_latex_table(
  rmsfe_latex_df[, c("model", "RMSFE", "MAE")],
  "Forecast accuracy on the test period (original scale)",
  "tab:project3_rmsfe",
  "lcc",
  c("Model", "RMSFE", "MAE")
)

writeLines(rmsfe_tex, file.path(table_dir, "project3_rmsfe_table.tex"))

### 9. Plot forecasts ---------------------------------------------------------
plot_forecasts <- function(df, title_text, file_prefix) {
  model_names <- unique(df$model)
  out_files <- character(length(model_names))

  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    sub <- df[df$model == model_name, ]
    safe_model <- gsub("[^A-Za-z0-9]+", "_", model_name)
    out_file <- file.path(plot_dir, paste0(file_prefix, "_", safe_model, ".png"))
    model_rmsfe <- rmse(sub$actual, sub$forecast)

    png(filename = out_file, width = 1400, height = 800, res = 150)
    par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))

    ylim <- range(c(sub$actual, sub$forecast), na.rm = TRUE)
    plot(sub$target_date, sub$actual,
      type = "l", col = "steelblue", lwd = 1,
      ylim = ylim,
      main = paste0(title_text, " - ", model_name, " | RMSFE = ", sprintf("%.2f", model_rmsfe)),
      xlab = "Date", ylab = "Daily cases"
    )
    lines(sub$target_date, sub$forecast, col = "firebrick", lwd = 1)
    legend("topleft", legend = c("Actual", "Forecast"), col = c("steelblue", "firebrick"), lty = 1, bty = "n", cex = 0.8)
    dev.off()

    out_files[i] <- out_file
  }

  out_files
}

plot_files_ar <- plot_forecasts(ar_roll, "AR benchmark forecast", "project3_ar_forecast")
plot_files_ar_taska <- plot_forecasts(ar_taska_roll, "Task a ARIMA forecast", "project3_ar_taska_forecast")
plot_files_sar <- plot_forecasts(sar_roll, "Seasonal AR forecast", "project3_sar_forecast")
plot_files_linear_static <- plot_forecasts(linear_static, "Task c linear forecasts (fixed training)", "project3_linear_static_forecasts")
plot_files_linear <- plot_forecasts(linear_roll, "Rolling linear forecasts with covariates", "project3_linear_forecasts")
plot_files_ml <- plot_forecasts(bind_rows(tree_lags_roll, tree_cov_roll, rf_lags_roll, rf_cov_roll), "Machine learning forecasts", "project3_ml_forecasts")
plot_files_all <- c(plot_files_ar, plot_files_ar_taska, plot_files_sar, plot_files_linear_static, plot_files_linear, plot_files_ml)

# Task-c combined figure: actual plus all single-covariate OLS forecasts in one plot.
linear_static_wide <- reshape(
  linear_static[, c("target_date", "model", "forecast")],
  timevar = "model",
  idvar = "target_date",
  direction = "wide"
)
linear_actual <- linear_static[linear_static$model == unique(linear_static$model)[1], c("target_date", "actual")]
linear_combined <- merge(linear_actual, linear_static_wide, by = "target_date", all.x = TRUE)

combined_file <- file.path(plot_dir, "project3_task_c_linear_combined.png")
png(filename = combined_file, width = 1600, height = 900, res = 150)
plot(
  linear_combined$target_date,
  linear_combined$actual,
  type = "l",
  col = "black",
  lwd = 1.4,
  xlab = "Date",
  ylab = "Daily cases",
  main = "Task c: Actual vs OLS forecasts (one covariate at a time)"
)
for (j in seq_along(names(linear_combined))) {
  n <- names(linear_combined)[j]
  if (grepl("^forecast\\.", n)) {
    lines(linear_combined$target_date, linear_combined[[n]], lwd = 1)
  }
}
legend(
  "topright",
  legend = c("Actual", gsub("^forecast\\.LM_", "LM_", names(linear_combined)[grepl("^forecast\\.", names(linear_combined))])),
  col = c("black", seq_len(sum(grepl("^forecast\\.", names(linear_combined)))) + 1),
  lty = 1,
  bty = "n",
  cex = 0.8
)
dev.off()
plot_files_all <- c(plot_files_all, combined_file)

### 10. Diebold-Mariano tests ------------------------------------------------
dm_test_pair <- function(df, model_1, model_2) {
  d1 <- df %>% filter(model == model_1) %>% select(target_date, error)
  d2 <- df %>% filter(model == model_2) %>% select(target_date, error)
  aligned <- inner_join(d1, d2, by = "target_date", suffix = c("_1", "_2"))
  if (nrow(aligned) == 0) stop("No overlapping forecast dates for DM test.")

  dm <- forecast::dm.test(aligned$error_1, aligned$error_2, h = 1, power = 2)
  data.frame(
    model_1 = model_1,
    model_2 = model_2,
    dm_statistic = unname(dm$statistic),
    p_value = dm$p.value,
    stringsAsFactors = FALSE
  )
}

best_linear_model <- rmsfe_table$model[grepl("^LM_", rmsfe_table$model)][1]
best_ml_model <- rmsfe_table$model[grepl("^(Tree|RF)_", rmsfe_table$model)][1]

dm_pairs <- list(
  c(ar_roll$model[1], best_linear_model),
  c(ar_roll$model[1], sar_roll$model[1]),
  c(ar_roll$model[1], best_ml_model),
  c(best_linear_model, best_ml_model),
  c(best_linear_model, sar_roll$model[1])
)

dm_results <- bind_rows(lapply(dm_pairs, function(pair) dm_test_pair(forecast_table, pair[1], pair[2]))) %>%
  arrange(p_value)

print(dm_results)
write.csv(dm_results, file.path(table_dir, "project3_dm_tests.csv"), row.names = FALSE)

# -----------------------------
# DM-test interpretation notes
# -----------------------------
# What is tested:
# - Diebold-Mariano (DM) compares predictive accuracy of two forecast models.
# - Null hypothesis: equal expected loss for both models.
# - Here the loss is squared forecast error (power = 2) with one-step horizon (h = 1).
#
# Which models are compared:
# - AR(7)_roll (common benchmark required by the course)
# - LM_all_roll (best linear model with all covariates)
# - SAR(5,2)[7]_roll (best seasonal benchmark from this script run)
# - RF_cov_roll (best ML model from this script run)
#
# How to read the results:
# - p-value < 0.05: reject equal predictive accuracy (statistically significant difference).
# - p-value >= 0.05: no statistically significant difference on this test sample.
#
# Conclusions from current output (project3_dm_tests.csv):
# - AR(7) vs LM_all: significant (p < 0.05), LM_all outperforms AR(7).
# - AR(7) vs SAR: significant (p < 0.05), SAR outperforms AR(7).
# - AR(7) vs RF_cov: not significant.
# - LM_all vs SAR: not significant.
# - LM_all vs RF_cov: not significant.
#
# Practical takeaway:
# - AR(7) is a useful baseline but is significantly weaker than stronger linear/seasonal variants.
# - Among top non-benchmark models, ranking by RMSFE exists, but pairwise DM evidence is not strong.

dm_latex_df <- dm_results %>%
  mutate(
    model_1 = fmt_model(model_1),
    model_2 = fmt_model(model_2),
    dm_statistic = sprintf("%.4f", dm_statistic),
    p_value = sprintf("%.4f", p_value)
  ) %>%
  as.data.frame(stringsAsFactors = FALSE)

dm_tex <- build_latex_table(
  dm_latex_df[, c("model_1", "model_2", "dm_statistic", "p_value")],
  "Diebold-Mariano test results (squared-error loss, h = 1)",
  "tab:project3_dm",
  "llcc",
  c("Model 1", "Model 2", "DM statistic", "p-value")
)

writeLines(dm_tex, file.path(table_dir, "project3_dm_tests.tex"))

### 11. Console summary -------------------------------------------------------
cat("\nRMSFE summary (sorted):\n")
print(rmsfe_table)

cat("\nDiebold-Mariano summary (sorted by p-value):\n")
print(dm_results)

cat("\nFiles written to:\n")
for (pf in plot_files_all) {
  cat(" -", pf, "\n")
}
cat(" -", file.path(table_dir, "project3_forecasts.csv"), "\n")
cat(" -", file.path(table_dir, "project3_rmsfe_table.csv"), "\n")
cat(" -", file.path(table_dir, "project3_bic_selection.tex"), "\n")
cat(" -", file.path(table_dir, "project3_rmsfe_table.tex"), "\n")
cat(" -", file.path(table_dir, "project3_dm_tests.csv"), "\n")
cat(" -", file.path(table_dir, "project3_dm_tests.tex"), "\n")

### Dm models against every model, heatplot to see the difference, see on the literature.
### Packages in R
### Provide all information of the test statistic, long run value estimator.
if (!require("dplyr", character.only = TRUE)) install.packages("dplyr"); library(dplyr)
if (!require("knitr", character.only = TRUE)) install.packages("knitr"); library(knitr)
if (!require("dplyr", character.only = TRUE)) install.packages("dplyr"); library(dplyr)
if (!require("tidyr", character.only = TRUE)) install.packages("tidyr"); library(tidyr)
if (!require("ggplot2", character.only = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!require("readr", character.only = TRUE)) install.packages("readr"); library(readr)
if (!require("forecast", character.only = TRUE)) install.packages("forecast"); library(forecast)
if (!require("zoo", character.only = TRUE)) install.packages("zoo"); library(zoo)
if (!require("patchwork", character.only = TRUE)) install.packages("patchwork"); library(patchwork)
if (!require("tseries", character.only = TRUE)) install.packages("tseries"); library(tseries)
if (!require("lmtest", character.only = TRUE)) install.packages("lmtest"); library(lmtest)
if (!require("jsonlite", character.only = TRUE)) install.packages("jsonlite"); library(jsonlite)
if (!require("future", character.only = TRUE)) install.packages("future"); library(future)
if (!require("future.apply", character.only = TRUE)) install.packages("future.apply"); library(future.apply)
if (!require("lubridate", character.only = TRUE)) install.packages("lubridate"); library(lubridate)

if (!require("rpart", character.only = TRUE)) install.packages("rpart"); library(rpart)
if (!require("rpart.plot", character.only = TRUE)) install.packages("rpart.plot"); library(rpart.plot)
if (!require("ranger", character.only = TRUE)) install.packages("ranger"); library(ranger)
##==============================================================================
##=============================0. Some Preparation======================================
##==============================================================================
#data_infektionen is downloaded from https://github.com/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland
#immunizations datasets are downloaded from https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/main/Deutschland_Impfquoten_COVID-19.csv

# Identify Working path
working_path <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Set the number of threads; keep it below the number of threads on your own computer.
threads_number <- 9 
# creat a folder to save plots
folder_name <- "RuishengShi_Plots"
if (!dir.exists(folder_name)) {dir.create(folder_name)}



##==============================================================================
##=============================1. Load Data=====================================
##==============================================================================

# Read the infektionen data.
data_infektionen <- read_csv("Aktuell_Deutschland_SarsCov2_Infektionen.csv")

# data cleaning
# 1. set date range
analysis_start <- as.Date("2020-04-01")
analysis_end   <- as.Date("2026-03-31")
data_infektionen <- data_infektionen %>%
  filter(Meldedatum >= analysis_start, Meldedatum <= analysis_end)

# 2. extract state code
data_infektionen <- data_infektionen %>%
  mutate(BundeslandId_Impfort = IdLandkreis %/% 1000, .after = IdLandkreis)





##==============================================================================
##=======================2. Compute Cases and Death data========================
## Using Meldedatum as the reference date, compute infection cases and deaths (recovered cases are not calculated for now because the assignment does not require them).
##==============================================================================

# Infections: sum AnzahnFall by Meldedatum
daily_cases <- data_infektionen %>%
  group_by(Meldedatum) %>%
  summarise(
    new_cases = sum(AnzahlFall, na.rm = TRUE),
    .groups = "drop"
  )

# Deaths: sum AnzahlTodesfall, but only for rows where a death was recorded
# Ignore NeuerTodesfall==-9 and -1
daily_deaths <- data_infektionen %>%                               
  filter(NeuerTodesfall %in% c(0L, 1L)) %>%                        # Keep death-status codes 0 and 1; exclude -9 and -1.
  group_by(Meldedatum) %>%                                         # Group by each day and aggregate to obtain daily death data.
  summarise(deaths = sum(AnzahlTodesfall), .groups = "drop") %>%   # Sum AnzahlTodesfall across all corresponding rows; .groups = "drop" removes the grouping.
  arrange(Meldedatum)                                              # Sort the table chronologically (starting from early 2020).


# ==============discovering missing values=================

fill_missing_dates <- function(df, date_col) {
  
  df_name <- deparse(substitute(df))
  
  df_complete <- df %>%
    complete(!!sym(date_col) := seq(as.Date("2020-04-01"), 
                                    as.Date("2026-03-31"), 
                                    by = "day")) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  
  # 3. count the number of missing values
  missing_count <- nrow(df_complete) - nrow(df)
  
  # 4. print out the results
  cat(missing_count, "missing values are discovered in", df_name, "\n")
  
  return(df_complete)
}

# fill missing values
daily_cases  <- fill_missing_dates(daily_cases, "Meldedatum")
daily_deaths <- fill_missing_dates(daily_deaths, "Meldedatum")






#===============================================================================
#-------------- 3. Data Processing: 
#-------------- Logarithm -> Seasonal differences(7) -> 1st order difference
#===============================================================================

preprocess_ts <- function(series) {
  # 1. Logarithm
  y_log <- log1p(series)
  
  # 2. Seasonal differences, elimates weekly period
  y_seasonal_diff <- diff(y_log, lag = 7)
  
  # 3. detrending
  y_final <- diff(y_seasonal_diff, lag = 1)
  
  return(y_final)
}


cases_processed <- preprocess_ts(daily_cases$new_cases)
deaths_processed <- preprocess_ts(daily_deaths$deaths)








#===============================================================================
#-------------- 4. Machine Learning Forecasts: Part (a)
#-------------- One-step-ahead forecasts using only lags of log cases
#-------------- Models: Regression Tree and Random Forest
#-------------- Split: 70% training, 10% validation, 20% test
#-------------- Maximal lag: 10
#===============================================================================


#===============================================================================
# a) 1. Define the forecasting target
#===============================================================================


# The forecasting target for the first sub-question is log(1 + daily cases).
y_log_cases_full <- log1p(daily_cases$new_cases)

# The date vector must correspond one-to-one with y_log_cases_full.
date_cases_full <- as.Date(daily_cases$Meldedatum)





#===============================================================================
# a) 2. Create supervised learning dataset with lags 1 to 10
#===============================================================================

make_lagged_dataset <- function(y, dates, max_lag = 10) {
  
  df <- data.frame(
    date = as.Date(dates),
    target = as.numeric(y)
  )
  
  # Generate lag1, lag2, ..., lag10.
  for (j in 1:max_lag) {
    df[[paste0("lag", j)]] <- dplyr::lag(df$target, n = j)
  }
  
  # Because lag10 needs the previous 10 observations, the first 10 rows contain NA and cannot be used for model training, so they are removed.
  df <- df %>%
    tidyr::drop_na()
  rownames(df) <- NULL
  return(df)
}


# The assignment specifies maximal lag n = 10.
max_lag <- 10

# Construct the supervised learning dataset for log cases.
ml_cases_data <- make_lagged_dataset(
  y = y_log_cases_full,
  dates = date_cases_full,
  max_lag = max_lag
)

# Check the data structure.
cat("Number of usable observations after creating lags:", nrow(ml_cases_data), "\n")
head(ml_cases_data)
tail(ml_cases_data)


# Predictor names: lag1 to lag10.
predictor_cols <- paste0("lag", 1:max_lag)

# Check whether the final modeling dataset has missing values.
cat("Number of missing values in ml_cases_data:", sum(is.na(ml_cases_data)), "\n")




#===============================================================================
# a) 3. Split data into 70% training, 10% validation, 20% test
#===============================================================================

# 70%：training set
# 10%: validation set, used for hyperparameter tuning.
# 20%: test set, used for final out-of-sample evaluation.

n_total <- nrow(ml_cases_data)

train_end <- floor(0.70 * n_total)
val_end   <- floor(0.80 * n_total)

train_idx <- 1:train_end
val_idx   <- (train_end + 1):val_end
test_idx  <- (val_end + 1):n_total

cat("Total usable observations:", n_total, "\n")
cat("Training observations:", length(train_idx), "\n")
cat("Validation observations:", length(val_idx), "\n")
cat("Test observations:", length(test_idx), "\n")




#===============================================================================
# a) 4. Define evaluation functions: MSFE and RMSE
#===============================================================================

# Forecast error:
# e_t = y_t - yhat_t
#
# MSFE:
# MSFE = mean(e_t^2)
#
# RMSE:
# RMSE = sqrt(MSFE)

msfe_fun <- function(actual, forecast) {
  mean((actual - forecast)^2, na.rm = TRUE)
}

rmse_fun <- function(actual, forecast) {
  sqrt(msfe_fun(actual, forecast))
}




#===============================================================================
# a) 5. Rolling one-step-ahead forecast function for Regression Tree
#===============================================================================

# This function is the core of the first sub-question.

# For each forecast point:
# 1. Use all data from 1 to i-1 to train the model.
# 2. Use the trained model to forecast point i.
# 3. Then move i one step forward.
# 4. Re-estimate the model.

rolling_forecast_rpart <- function(data, start_idx, end_idx,
                                   predictor_cols,
                                   cp,
                                   maxdepth,
                                   minsplit = 20) {
  
  # data: the complete supervised learning dataset.
  # start_idx: row number where forecasting starts.
  # end_idx: row number where forecasting ends.
  # predictor_cols: predictor names, here lag1 to lag10.
  # cp: the complexity parameter in rpart.
  # maxdepth: maximum depth of the tree.
  # minsplit: minimum number of observations required in a node to allow further splitting.
  
  forecast_idx <- start_idx:end_idx
  
  forecasts <- numeric(length(forecast_idx))
  
  counter <- 1
  
  for (i in forecast_idx) {
    
    train_data_i <- data[1:(i - 1), c("target", predictor_cols), drop = FALSE]
    
    # Row i is the current observation to be forecast.
    new_data_i <- data[i, predictor_cols, drop = FALSE]
    
    # Estimate the regression tree.
    # method = "anova" means predicting a continuous variable, i.e., a regression tree.
    # xval = 0 turns off rpart's internal cross-validation.
    # This is because we have already set up our own time-series validation set.
    fit_i <- rpart(
      formula = target ~ .,
      data = train_data_i,
      method = "anova",
      control = rpart.control(
        cp = cp,
        maxdepth = maxdepth,
        minsplit = minsplit,
        xval = 0
      )
    )
    
    # One-step-ahead forecast.
    forecasts[counter] <- as.numeric(predict(fit_i, newdata = new_data_i))
    
    counter <- counter + 1
  }
  
  result <- data.frame(
    row_id = forecast_idx,
    date = data$date[forecast_idx],
    actual = data$target[forecast_idx],
    forecast = forecasts
  )
  
  return(result)
}





#===============================================================================
# a) 6. Rolling one-step-ahead forecast function for Random Forest
#===============================================================================

# Random Forest also must be re-estimated at every step.
#
# For each forecast point i:
#
# 1. Use all data from 1 to i-1 to train the random forest.
# 2. Forecast point i.
# 3. Expand the training window in the next step.
# 4. Re-estimate the random forest.

rolling_forecast_ranger <- function(data, start_idx, end_idx,
                                    predictor_cols,
                                    mtry,
                                    min.node.size,
                                    num.trees = 300,
                                    seed = 2026,
                                    num.threads = 1) {
  
  # data: the complete supervised learning data.
  # start_idx, end_idx: forecasting interval.
  # predictor_cols: lag1 to lag10.
  # mtry: number of variables randomly considered at each split.
  # min.node.size: minimum sample size of leaf nodes.
  # num.trees: number of trees in the forest.
  # seed: random seed.
  # num.threads: number of threads used by ranger.
  
  forecast_idx <- start_idx:end_idx
  
  forecasts <- numeric(length(forecast_idx))
  
  counter <- 1
  
  for (i in forecast_idx) {
    
    # The training data can only include data before the forecast point.
    train_data_i <- data[1:(i - 1), c("target", predictor_cols), drop = FALSE]
    
    # The current observation to be forecast.
    new_data_i <- data[i, predictor_cols, drop = FALSE]
    
    # mtry cannot exceed the number of predictors.
    mtry_i <- min(mtry, length(predictor_cols))
    
    # Estimate the random forest.
    fit_i <- ranger(
      formula = target ~ .,
      data = train_data_i,
      num.trees = num.trees,
      mtry = mtry_i,
      min.node.size = min.node.size,
      seed = seed + i,
      num.threads = num.threads
    )
    
    # One-step-ahead forecast.
    pred_i <- predict(fit_i, data = new_data_i)$predictions
    
    forecasts[counter] <- as.numeric(pred_i)
    
    counter <- counter + 1
  }
  
  result <- data.frame(
    row_id = forecast_idx,
    date = data$date[forecast_idx],
    actual = data$target[forecast_idx],
    forecast = forecasts
  )
  
  return(result)
}






#===============================================================================
# a) 7. Hyperparameter tuning for Regression Tree using validation set
# Use the validation set to choose the best parameters for the regression tree.
#===============================================================================

# Main tuning parameters for the Regression Tree:
#
# cp:
#   complexity parameter
#   Smaller cp makes the tree more complex; splits are allowed even when the error improvement is small.
#   Larger cp makes the tree simpler; further splitting is allowed only when the split brings a sufficiently large error improvement.
#
# maxdepth: maximum tree depth; a larger maxdepth makes the tree more complex.
#   
# Note:
# Use the validation set for tuning here; do not use the test set for tuning.

tree_grid <- expand.grid(
  cp = c(0.0005, 0.001, 0.005),
  maxdepth = c(5, 7, 10),
  minsplit = c(10, 20, 30),
  KEEP.OUT.ATTRS = FALSE
)

tree_tuning_results <- data.frame()

cat("\nStart tuning Regression Tree...\n")

for (g in 1:nrow(tree_grid)) {
  
  cat("Regression Tree tuning:", g, "of", nrow(tree_grid), "\n")
  
  cp_g <- tree_grid$cp[g]
  maxdepth_g <- tree_grid$maxdepth[g]
  minsplit_g <- tree_grid$minsplit[g]
  
  # validation rolling forecast
  # The first validation point is train_end + 1.
  # The last validation point is val_end.
  val_forecast_g <- rolling_forecast_rpart(
    data = ml_cases_data,
    start_idx = train_end + 1,
    end_idx = val_end,
    predictor_cols = predictor_cols,
    cp = cp_g,
    maxdepth = maxdepth_g,
    minsplit = minsplit_g
  )
  
  val_msfe_g <- msfe_fun(actual = val_forecast_g$actual,forecast = val_forecast_g$forecast)
  val_rmse_g <- rmse_fun(actual = val_forecast_g$actual,forecast = val_forecast_g$forecast)
  
  tree_tuning_results <- rbind(
    tree_tuning_results,
    data.frame(
      cp = cp_g,
      maxdepth = maxdepth_g,
      minsplit = minsplit_g,
      validation_MSFE = val_msfe_g,
      validation_RMSE = val_rmse_g
    )
  )
}

# Sort by validation RMSE in ascending order.
tree_tuning_results <- tree_tuning_results %>%
  arrange(validation_RMSE)

cat("\nRegression Tree tuning results:\n")
print(tree_tuning_results)

# Select the parameters with the smallest validation RMSE.
best_tree_params <- tree_tuning_results[1, ]

cat("\nBest Regression Tree parameters:\n")
print(best_tree_params)






#===============================================================================
# a) 8. Hyperparameter tuning for Random Forest using validation set
# Use the validation set to choose the best parameters for the random forest.
#===============================================================================

# Main tuning parameters for Random Forest:
# mtry: number of variables randomly considered at each split.
# min.node.size: minimum sample size of leaf nodes; smaller values make the model more complex.
# num.trees: number of trees in the forest; fixed at 300 here; larger values make the model more stable.

# mtry * min.node.size = 12, so there are 12 parameter combinations in total.


# Because we only have lag1 to lag10, the number of predictors is 10.
# mtry cannot exceed 10.

# To avoid occupying the computer for too long, do not set the grid too large here.
# If your computer runs slowly, you can further reduce the candidate values for mtry or min.node.size.

rf_grid <- expand.grid(
  mtry = c(2, 3, 5, 10),   # Indicates how many variables are considered at each split; for example, mtry = 10 considers all variables.
  min.node.size = c(5, 10, 20),
  num.trees = c(300),
  KEEP.OUT.ATTRS = FALSE
)

rf_tuning_results <- data.frame()

# ranger can use multiple threads.
# However, because many models are repeatedly estimated here, do not set this too aggressively.
# If your computer has 4 cores and 8 threads, setting this to 2 or 4 is usually fine.
n_threads_ranger <- min(threads_number, max(1, parallel::detectCores() - 1))

cat("\nNumber of threads used by ranger:", n_threads_ranger, "\n")
cat("\nStart tuning Random Forest...\n")

for (g in 1:nrow(rf_grid)) {
  
  cat("Random Forest tuning:", g, "of", nrow(rf_grid), "\n")
  
  mtry_g <- rf_grid$mtry[g]
  min_node_g <- rf_grid$min.node.size[g]
  num_trees_g <- rf_grid$num.trees[g]
  
  # validation rolling forecast
  rf_val_forecast_g <- rolling_forecast_ranger(
    data = ml_cases_data,
    start_idx = train_end + 1,
    end_idx = val_end,
    predictor_cols = predictor_cols,
    mtry = mtry_g,
    min.node.size = min_node_g,
    num.trees = num_trees_g,
    seed = 2026,
    num.threads = n_threads_ranger
  )
  
  # RMSE and MSFE.
  val_msfe_g <- msfe_fun(actual = rf_val_forecast_g$actual,forecast = rf_val_forecast_g$forecast)
  val_rmse_g <- rmse_fun(actual = rf_val_forecast_g$actual,forecast = rf_val_forecast_g$forecast)
  
  rf_tuning_results <- rbind(
    rf_tuning_results,
    data.frame(
      mtry = mtry_g,
      min.node.size = min_node_g,
      num.trees = num_trees_g,
      validation_MSFE = val_msfe_g,
      validation_RMSE = val_rmse_g
    )
  )
}

# Sort by validation RMSE in ascending order.
rf_tuning_results <- rf_tuning_results %>%
  arrange(validation_RMSE)

cat("\nRandom Forest tuning results:\n")
print(rf_tuning_results)

# Select the parameters with the smallest validation RMSE.
best_rf_params <- rf_tuning_results[1, ]

cat("\nBest Random Forest parameters:\n")
print(best_rf_params)





#===============================================================================
# a) 9. Final test forecasts using best hyperparameters
# Finally, examine out-of-sample forecasting performance on the test set.
#===============================================================================

# After tuning is complete, we fix the best hyperparameters.
# Then we conduct the true out-of-sample evaluation on the test set.
#
# The first test point is val_end + 1.
# That is, the first test forecast can use the first 80% of the data as its training sample.
#
# For each test point:
# Re-estimate the model using all observations before it.
# Then forecast the current point.

cat("\nStart final test forecast for Regression Tree...\n")

tree_test_forecast <- rolling_forecast_rpart(
  data = ml_cases_data,
  start_idx = val_end + 1,
  end_idx = n_total,
  predictor_cols = predictor_cols,
  cp = best_tree_params$cp,
  maxdepth = best_tree_params$maxdepth,
  minsplit = best_tree_params$minsplit
)

cat("\nStart final test forecast for Random Forest...\n")

rf_test_forecast <- rolling_forecast_ranger(
  data = ml_cases_data,
  start_idx = val_end + 1,
  end_idx = n_total,
  predictor_cols = predictor_cols,
  mtry = best_rf_params$mtry,
  min.node.size = best_rf_params$min.node.size,
  num.trees = best_rf_params$num.trees,
  seed = 2026,
  num.threads = n_threads_ranger
)





#===============================================================================
# a) 10. Compute test MSFE and RMSE of Regression Tree and Ramdom Forest
#===============================================================================

tree_test_msfe <- msfe_fun(
  actual = tree_test_forecast$actual,
  forecast = tree_test_forecast$forecast
)

tree_test_rmse <- rmse_fun(
  actual = tree_test_forecast$actual,
  forecast = tree_test_forecast$forecast
)

rf_test_msfe <- msfe_fun(
  actual = rf_test_forecast$actual,
  forecast = rf_test_forecast$forecast
)

rf_test_rmse <- rmse_fun(
  actual = rf_test_forecast$actual,
  forecast = rf_test_forecast$forecast
)


# Summarize the final results.
part_a_results <- data.frame(
  Model = c("Regression Tree", "Random Forest"),
  Predictors = c("log cases lags 1-10", "log cases lags 1-10"),
  Test_MSFE = c(tree_test_msfe, rf_test_msfe),
  Test_RMSE = c(tree_test_rmse, rf_test_rmse)
)

cat("\nFinal test results for Part (a):\n")
# Use knitr to output a cleaner table:
knitr::kable(part_a_results, digits = 6)





#===============================================================================
# a) 11. Combine actual values and forecasts into one dataframe
#===============================================================================

test_forecasts_part_a <- data.frame(
  date = tree_test_forecast$date,
  actual_log_cases = tree_test_forecast$actual,
  regression_tree_forecast = tree_test_forecast$forecast,
  random_forest_forecast = rf_test_forecast$forecast
)




#===============================================================================
# a) 12. Plot actual log cases and forecasts in the test period
#===============================================================================
test_forecasts_long <- test_forecasts_part_a %>%
  tidyr::pivot_longer(
    cols = c(actual_log_cases, regression_tree_forecast, random_forest_forecast),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = dplyr::recode(
      series,
      actual_log_cases = "Actual log cases",
      regression_tree_forecast = "Regression Tree forecast",
      random_forest_forecast = "Random Forest forecast"
    )
  )

# Generate the MSFE / RMSE text displayed in the plot.
performance_text <- sprintf(
  "Regression Tree: MSFE = %.4f, RMSE = %.4f\nRandom Forest: MSFE = %.4f, RMSE = %.4f",
  tree_test_msfe, tree_test_rmse,
  rf_test_msfe, rf_test_rmse
)

# Set the annotation position in the upper-left corner of the plot.
x_pos <- min(test_forecasts_long$date, na.rm = TRUE)
y_pos <- max(test_forecasts_long$value, na.rm = TRUE)
plot_part_a_forecasts <- ggplot(
  test_forecasts_long,
  aes(x = date, y = value, color = series)
) +
  geom_line(linewidth = 0.7) +
  
  # Add two lines of MSFE / RMSE results to the plot.
  annotate(
    "label",
    x = x_pos,
    y = y_pos,
    label = performance_text,
    hjust = 0,
    vjust = 1,
    size = 3.5,
    color = "black",
    fill = "white",
    label.size = 0.25
  ) +
  
  # Manually adjust the colors.
  scale_color_manual(
    values = c(
      "Actual log cases" = "gray80",        # Or change this to "grey20".
      "Regression Tree forecast" = "orange",
      "Random Forest forecast" = "#0072B2"
    )
  ) +
  
  labs(
    title = "One-step-ahead forecasts of log cases: Regression Tree vs Random Forest",
    subtitle = "Predictors: lags 1 to 10 of log cases",
    x = "Date",
    y = "log(1 + daily cases)",
    color = "Series"
  ) +
  theme_minimal()

print(plot_part_a_forecasts)

ggsave(
  filename = file.path(folder_name, "1.One_step_ahead_forecasts_tree_rf.png"),
  plot = plot_part_a_forecasts,
  width = 10,
  height = 6,
  dpi = 200
)






















#===============================================================================
#-------------- Build additional predictors for Part (b)
#-------------- Additional regressors:
#-------------- 1. Temperature
#-------------- 2. Share of cases aged 60+
#-------------- 3. Share of cases aged 80+
#-------------- 4. Start-of-week dummy
#===============================================================================


#===============================================================================
# b) 1. Build age-based predictors from the RKI infection dataset
#===============================================================================
# Definition of the age variables:
# share_cases_60plus_t = infections aged 60+ on day t / total infections on day t.
# share_cases_80plus_t = infections aged 80+ on day t / total infections on day t.
#
# This is not the absolute number of cases aged 60+, because absolute counts are themselves part of total cases and highly overlap with the target variable.
# The share is the infection age composition.


# First check which age groups appear in Altersgruppe.
cat("\nAll of age groups in the RKI data:\n")
print(unique(data_infektionen$Altersgruppe))


covid_age <- data_infektionen %>%
  mutate(
    # RKI age groups are generally similar to:
    # A00-A04, A05-A14, A15-A34, A35-A59, A60-A79, A80+
    #
    # "^A(6|7|8|9)" means:
    # Altersgruppe starts with A6, A7, A8, or A9.
    # Therefore, it can identify age groups aged 60 and above.
    is_60plus = grepl("^A(6|7|8|9)", Altersgruppe),
    
    # "^A8" is used to identify the A80+ age group.
    is_80plus = grepl("^A8", Altersgruppe)
  )


daily_age_cases <- covid_age %>%
  group_by(Meldedatum) %>%
  summarise(
    # Total infections each day.
    cases_total = sum(AnzahlFall, na.rm = TRUE),
    
    # Infections aged 60+ each day.
    cases_60plus = sum(AnzahlFall[is_60plus], na.rm = TRUE),
    
    # Infections aged 80+ each day.
    cases_80plus = sum(AnzahlFall[is_80plus], na.rm = TRUE),
    
    # Daily share of infected people aged 60+.
    # If cases_total = 0 on a day, set the share to 0 to avoid division by zero.
    share_cases_60plus = ifelse(
      cases_total > 0,
      cases_60plus / cases_total,
      0
    ),
    
    # Daily share of infected people aged 80+.
    share_cases_80plus = ifelse(
      cases_total > 0,
      cases_80plus / cases_total,
      0
    ),
    
    .groups = "drop"
  ) %>%
  arrange(Meldedatum)


# Simple check of the age variables.
cat("\nSummary of age-based case predictors:\n")
print(summary(daily_age_cases[, c("share_cases_60plus", "share_cases_80plus")]))


#===============================================================================
# b) 2. Download Germany daily mean temperature from Open-Meteo
#===============================================================================

# This step constructs the daily temperature variable temp_c.
#
# Note:
# The approximate center-point coordinates of Germany are used here:
# latitude  = 51.1657
# longitude = 10.4515
#
# Therefore, strictly speaking, this is the daily mean temperature at a representative central location in Germany,
# not the true national average temperature weighted across all German regions.
# However, it is acceptable as a temperature covariate for this project.


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

daily_temp <- daily_temp %>%
  arrange(Meldedatum) %>%
  mutate(
    # If there are a few missing values in the middle of the temperature series, fill them with linear interpolation.
    temp_c = zoo::na.approx(temp_c, na.rm = FALSE),
    
    # If there are still NA values at the beginning, fill them backward using the nearest later value.
    temp_c = zoo::na.locf(temp_c, na.rm = FALSE),
    
    # If there are still NA values at the end, fill them forward using the nearest earlier value.
    temp_c = zoo::na.locf(temp_c, fromLast = TRUE, na.rm = FALSE)
  )


# Simple check of the temperature variable.
cat("\nSummary of temperature predictor:\n")
print(summary(daily_temp$temp_c))


#===============================================================================
# b) 3. Build the final daily predictor table for Part (b)
#===============================================================================

# This step merges all additional covariates into a daily-level predictor table.
#
# The final additional covariates retained are:
#
# temp_c:
#   Daily average temperature.
#
# share_cases_60plus:
#   Daily share of infected people aged 60+.
#
# share_cases_80plus:
#   Daily share of infected people aged 80+.
#
# is_start_week:
#   start-of-week dummy
#   Defined here as a Monday dummy.
#
# Note:
# Deaths-related variables are not included here.
# time_idx is also not included.
# These variables are also not double-differenced.
#
# Later, when doing machine-learning forecasts, we construct lags 1 to 10 for all these variables.


predictors_daily <- data.frame(Meldedatum = daily_cases$Meldedatum) %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  left_join(
    daily_age_cases %>%
      select(Meldedatum, share_cases_60plus, share_cases_80plus),
    by = "Meldedatum"
  ) %>%
  arrange(Meldedatum) %>%
  mutate(
    # If some dates have no age-structure information, set the values to 0.
    # This is because these dates usually correspond to days with no reported cases.
    share_cases_60plus = replace(
      share_cases_60plus,
      is.na(share_cases_60plus),
      0
    ),
    
    share_cases_80plus = replace(
      share_cases_80plus,
      is.na(share_cases_80plus),
      0
    ),
    
    # Use lubridate::wday to construct weekday variables and avoid effects from the system language setting.
    #
    # week_start = 1 means the week starts on Monday.
    # Therefore:
    # Monday    -> 1
    # Tuesday   -> 2
    # ...
    # Sunday    -> 7
    weekday_num = lubridate::wday(Meldedatum, week_start = 1),
    
    # start-of-week dummy
    # Defined here as a Monday dummy.
    is_start_week = as.integer(weekday_num == 1)
  ) %>%
  select(
    Meldedatum,
    temp_c,
    share_cases_60plus,
    share_cases_80plus,
    is_start_week
  )




#===============================================================================
# b) 4. Define the additional regressors for cases forecasts
#===============================================================================

# Additional regressors to be included in the second sub-question:
#
# 1. temp_c
# 2. share_cases_60plus
# 3. share_cases_80plus
# 4. is_start_week
#
# Note:
# The result here is daily-level raw covariates.
# We cannot directly use X_t to forecast y_t later.
# Later we must construct:
#
# temp_c_lag1, ..., temp_c_lag10
# share_cases_60plus_lag1, ..., share_cases_60plus_lag10
# share_cases_80plus_lag1, ..., share_cases_80plus_lag10
# is_start_week_lag1, ..., is_start_week_lag10
#
# This is how we satisfy the assignment requirement:
# "Make sure you lag them."


covariate_cols_part_b <- c(
  "temp_c",
  "share_cases_60plus",
  "share_cases_80plus",
  "is_start_week"
)

xreg_cases_raw <- as.matrix(
  predictors_daily[, covariate_cols_part_b]
)

# Add row names to the matrix for easier checking later.
rownames(xreg_cases_raw) <- as.character(predictors_daily$Meldedatum)


cat("\nDimension of xreg_cases_raw:\n")
print(dim(xreg_cases_raw))

cat("\nColumn names of xreg_cases_raw:\n")
print(colnames(xreg_cases_raw))

cat("\nFirst rows of xreg_cases_raw:\n")
print(head(xreg_cases_raw))





































#===============================================================================
# b) 5 Create supervised learning dataset with lagged y and lagged covariates
#===============================================================================

# Objective of this function:
#
# Transform the raw time-series data into a table usable for machine learning.
#
# Raw data:
#
# date_t, y_t, temp_t, share60_t, share80_t, startweek_t
#
# After transformation:
#
# target_t = y_t
#
# predictors:
#
# y_lag1_t      = y_{t-1}
# y_lag2_t      = y_{t-2}
# ...
# y_lag10_t     = y_{t-10}
#
# temp_c_lag1_t = temp_{t-1}
# temp_c_lag2_t = temp_{t-2}
# ...
# temp_c_lag10_t = temp_{t-10}
#
# share_cases_60plus_lag1_t = share60_{t-1}
# ...
# share_cases_60plus_lag10_t = share60_{t-10}
#
# share_cases_80plus_lag1_t = share80_{t-1}
# ...
# share_cases_80plus_lag10_t = share80_{t-10}
#
# is_start_week_lag1_t = is_start_week_{t-1}
# ...
# is_start_week_lag10_t = is_start_week_{t-10}
#
# Key point:
# When forecasting target_t = y_t, we cannot use X_t; we can only use X_{t-1}, ..., X_{t-10}.
# This is the meaning of "Make sure you lag them" in the assignment.


make_lagged_dataset_with_covariates <- function(y, dates, xreg, max_lag = 10) {
  
  xreg_df <- as.data.frame(xreg)
  
  # To avoid special characters in column names, convert them all to valid R variable names.
  colnames(xreg_df) <- make.names(colnames(xreg_df))
  
  df <- data.frame(
    date = as.Date(dates),
    target = as.numeric(y),
    xreg_df,
    check.names = FALSE
  )
  
  # 1. Construct lags 1 to 10 for log cases.
  for (j in 1:max_lag) {
    df[[paste0("y_lag", j)]] <- dplyr::lag(df$target, n = j)
  }
  
  # 2. Construct lags 1 to 10 for each additional covariate.
  xreg_cols <- colnames(xreg_df)
  
  for (xname in xreg_cols) {
    for (j in 1:max_lag) {
      new_name <- paste0(xname, "_lag", j)
      df[[new_name]] <- dplyr::lag(df[[xname]], n = j)
    }
  }
  
  # 3. Keep only the variables needed for modeling.
  # Do not keep contemporaneous xreg values; that is, do not keep current values such as temp_c or share_cases_60plus.
  # This is because X_t cannot be used when forecasting y_t.
  lagged_y_cols <- paste0("y_lag", 1:max_lag)
  
  lagged_x_cols <- unlist(
    lapply(
      xreg_cols,
      function(xname) paste0(xname, "_lag", 1:max_lag)
    )
  )
  
  final_df <- df %>%
    select(
      date,
      target,
      all_of(lagged_y_cols),
      all_of(lagged_x_cols)
    ) %>%
    drop_na()
  
  rownames(final_df) <- NULL
  
  return(final_df)
}


# The assignment specifies maximal lag n = 10.
max_lag <- 10

# Construct the modeling data for sub-question b.
ml_cases_data_part_b <- make_lagged_dataset_with_covariates(
  y = y_log_cases_full,
  dates = date_cases_full,
  xreg = xreg_cases_raw,
  max_lag = max_lag
)

cat("\nNumber of usable observations in Part (b):", nrow(ml_cases_data_part_b), "\n")



# All predictor columns:
# y_lag1 to y_lag10.
# temp_c_lag1 to temp_c_lag10.
# share_cases_60plus_lag1 to share_cases_60plus_lag10.
# share_cases_80plus_lag1 to share_cases_80plus_lag10.
# is_start_week_lag1 to is_start_week_lag10.

predictor_cols_part_b <- setdiff(
  colnames(ml_cases_data_part_b),
  c("date", "target")
)

cat("\nNumber of predictors in Part (b):", length(predictor_cols_part_b), "\n")
cat("\nPredictor names in Part (b):\n")
print(predictor_cols_part_b)




#===============================================================================
# b) 6 Split data into 70% training, 10% validation, 20% test
#===============================================================================

# Note:
# The data is split after constructing the lagged dataset.
#
# Because the first 10 observations are removed due to lag construction,
# the training / validation / test split should be based on
# the usable sample in ml_cases_data_part_b.

n_total_b <- nrow(ml_cases_data_part_b)

train_end_b <- floor(0.70 * n_total_b)
val_end_b   <- floor(0.80 * n_total_b)

train_idx_b <- 1:train_end_b
val_idx_b   <- (train_end_b + 1):val_end_b
test_idx_b  <- (val_end_b + 1):n_total_b

# Number of samples in the training, validation, and test sets.
cat("\nTotal usable observations in Part (b):", n_total_b, "\n")
cat("Training observations:", length(train_idx_b), "\n")
cat("Validation observations:", length(val_idx_b), "\n")
cat("Test observations:", length(test_idx_b), "\n")

# Time ranges of the training, validation, and test sets.
cat("\nTraining period:", as.character(ml_cases_data_part_b$date[min(train_idx_b)]), "to",as.character(ml_cases_data_part_b$date[max(train_idx_b)]), "\n")
cat("Validation period:", as.character(ml_cases_data_part_b$date[min(val_idx_b)]), "to",as.character(ml_cases_data_part_b$date[max(val_idx_b)]), "\n")
cat("Test period:", as.character(ml_cases_data_part_b$date[min(test_idx_b)]), "to",as.character(ml_cases_data_part_b$date[max(test_idx_b)]), "\n")



#===============================================================================
# b) 7 Rolling one-step-ahead forecast function for Regression Tree
#===============================================================================

rolling_forecast_rpart_b <- function(data, start_idx, end_idx,
                                     predictor_cols,
                                     cp,
                                     maxdepth,
                                     minsplit = 20) {
  
  forecast_idx <- start_idx:end_idx
  
  forecasts <- numeric(length(forecast_idx))
  
  counter <- 1
  
  for (i in forecast_idx) {
    
    # Use all data before the current forecast point as the training set.
    train_data_i <- data[1:(i - 1), c("target", predictor_cols), drop = FALSE]
    
    # Predictor values for the current forecast point.
    # Note: these predictor values are already all lagged variables.
    new_data_i <- data[i, predictor_cols, drop = FALSE]
    
    # Estimate the regression tree.
    fit_i <- rpart(
      formula = target ~ .,
      data = train_data_i,
      method = "anova",
      control = rpart.control(
        cp = cp,
        maxdepth = maxdepth,
        minsplit = minsplit,
        xval = 0
      )
    )
    
    # One-step-ahead forecast.
    forecasts[counter] <- as.numeric(
      predict(fit_i, newdata = new_data_i)
    )
    
    counter <- counter + 1
  }
  
  result <- data.frame(
    row_id = forecast_idx,
    date = data$date[forecast_idx],
    actual = data$target[forecast_idx],
    forecast = forecasts
  )
  
  return(result)
}


#===============================================================================
# b) 8. Rolling one-step-ahead forecast function for Random Forest
#===============================================================================

rolling_forecast_ranger_b <- function(data, start_idx, end_idx,
                                      predictor_cols,
                                      mtry,
                                      min.node.size,
                                      num.trees = 300,
                                      seed = 2026,
                                      num.threads = 1) {
  
  forecast_idx <- start_idx:end_idx
  
  forecasts <- numeric(length(forecast_idx))
  
  counter <- 1
  
  for (i in forecast_idx) {
    
    # Use all data before the current forecast point as the training set.
    train_data_i <- data[1:(i - 1), c("target", predictor_cols), drop = FALSE]
    
    # Lagged predictors for the current forecast point.
    new_data_i <- data[i, predictor_cols, drop = FALSE]
    
    # mtry cannot exceed the number of predictors.
    mtry_i <- min(mtry, length(predictor_cols))
    
    # Estimate the random forest.
    fit_i <- ranger(
      formula = target ~ .,
      data = train_data_i,
      num.trees = num.trees,
      mtry = mtry_i,
      min.node.size = min.node.size,
      seed = seed + i,
      num.threads = num.threads
    )
    
    # One-step-ahead forecast.
    pred_i <- predict(fit_i, data = new_data_i)$predictions
    
    forecasts[counter] <- as.numeric(pred_i)
    
    counter <- counter + 1
  }
  
  result <- data.frame(
    row_id = forecast_idx,
    date = data$date[forecast_idx],
    actual = data$target[forecast_idx],
    forecast = forecasts
  )
  
  return(result)
}


#===============================================================================
# b) 9. Hyperparameter tuning for Regression Tree using validation set
#===============================================================================

# Main hyperparameters of the Regression Tree:
#
# cp:
#   complexity parameter
#   Smaller cp makes the tree more complex.
#   Larger cp makes the tree simpler.
#
# maxdepth:
#   Maximum depth of the tree.
#
# minsplit:
#   Minimum number of observations required in a node to allow further splitting.
#
# To avoid overly slow computation, only cp and maxdepth are tuned here, while minsplit is fixed at 20.

tree_grid_b <- expand.grid(
  cp = c(0.0005, 0.001, 0.005),
  maxdepth = c(5, 7, 10),
  minsplit = c(10, 20, 30),
  KEEP.OUT.ATTRS = FALSE
)

tree_tuning_results_b <- data.frame()

cat("\nStart tuning Regression Tree for Part (b)...\n")

for (g in 1:nrow(tree_grid_b)) {
  
  cat("Regression Tree Part (b) tuning:", g, "of", nrow(tree_grid_b), "\n")
  
  cp_g <- tree_grid_b$cp[g]
  maxdepth_g <- tree_grid_b$maxdepth[g]
  minsplit_g <- tree_grid_b$minsplit[g]
  
  # validation rolling one-step-ahead forecast
  val_forecast_g <- rolling_forecast_rpart_b(
    data = ml_cases_data_part_b,
    start_idx = train_end_b + 1,
    end_idx = val_end_b,
    predictor_cols = predictor_cols_part_b,
    cp = cp_g,
    maxdepth = maxdepth_g,
    minsplit = minsplit_g
  )
  
  val_msfe_g <- msfe_fun(
    actual = val_forecast_g$actual,
    forecast = val_forecast_g$forecast
  )
  
  val_rmse_g <- rmse_fun(
    actual = val_forecast_g$actual,
    forecast = val_forecast_g$forecast
  )
  
  tree_tuning_results_b <- rbind(
    tree_tuning_results_b,
    data.frame(
      cp = cp_g,
      maxdepth = maxdepth_g,
      minsplit = minsplit_g,
      validation_MSFE = val_msfe_g,
      validation_RMSE = val_rmse_g
    )
  )
}

tree_tuning_results_b <- tree_tuning_results_b %>%
  arrange(validation_RMSE)%>%
  mutate(cp = sprintf("%.4f", cp))


cat("\nRegression Tree tuning results for Part (b):\n")
print(tree_tuning_results_b)

best_tree_params_b <- tree_tuning_results_b[1, ]

cat("\nBest Regression Tree parameters for Part (b):\n")
print(best_tree_params_b)


#===============================================================================
# b) 10. Hyperparameter tuning for Random Forest using validation set
#===============================================================================

# The number of predictors in Part (b) increases noticeably:
#
# y lags: 10
# temp lags: 10
# share_cases_60plus lags: 10
# share_cases_80plus lags: 10
# is_start_week lags: 10
#
# There are roughly 50 predictors in total.
#
# Main tuning parameters for Random Forest:
#
# mtry:
#   Number of variables randomly considered at each split.
#
# min.node.size:
#   Minimum sample size of leaf nodes.
#
# num.trees:
#   Number of trees.
#
# To control computation time, num.trees is fixed at 300 here.
# If it runs too slowly, num.trees can be changed to 100 or 200.
# If more stability is needed in the final version, num.trees can be changed to 500.

p_b <- length(predictor_cols_part_b)

rf_grid_b <- expand.grid(
  mtry = unique(pmin(c(10, 15,floor(sqrt(p_b)),floor(p_b / 3),floor(p_b / 2)), p_b)), # I found that 5 and 7 have poor forecasting performance, so they are omitted.
  min.node.size = c(5, 10, 20),
  num.trees = c(300),
  KEEP.OUT.ATTRS = FALSE
)

rf_grid_b <- rf_grid_b %>%
  arrange(mtry, min.node.size)

cat("\nRandom Forest tuning grid for Part (b):\n")
print(rf_grid_b)

rf_tuning_results_b <- data.frame()

# Use multiple threads in ranger.
# If your computer has 4 cores and 8 threads, 2 to 4 is usually a stable choice.
n_threads_ranger_b <- min(threads_number, max(1, parallel::detectCores() - 1))

cat("\nNumber of threads used by ranger in Part (b):", n_threads_ranger_b, "\n")
cat("\nStart tuning Random Forest for Part (b)...\n")

for (g in 1:nrow(rf_grid_b)) {
  
  cat("Random Forest Part (b) tuning:", g, "of", nrow(rf_grid_b), "\n")
  
  mtry_g <- rf_grid_b$mtry[g]
  min_node_g <- rf_grid_b$min.node.size[g]
  num_trees_g <- rf_grid_b$num.trees[g]
  
  # validation rolling one-step-ahead forecast
  rf_val_forecast_g <- rolling_forecast_ranger_b(
    data = ml_cases_data_part_b,
    start_idx = train_end_b + 1,
    end_idx = val_end_b,
    predictor_cols = predictor_cols_part_b,
    mtry = mtry_g,
    min.node.size = min_node_g,
    num.trees = num_trees_g,
    seed = 2026,
    num.threads = n_threads_ranger_b
  )
  
  val_msfe_g <- msfe_fun(
    actual = rf_val_forecast_g$actual,
    forecast = rf_val_forecast_g$forecast
  )
  
  val_rmse_g <- rmse_fun(
    actual = rf_val_forecast_g$actual,
    forecast = rf_val_forecast_g$forecast
  )
  
  rf_tuning_results_b <- rbind(
    rf_tuning_results_b,
    data.frame(
      mtry = mtry_g,
      min.node.size = min_node_g,
      num.trees = num_trees_g,
      validation_MSFE = val_msfe_g,
      validation_RMSE = val_rmse_g
    )
  )
}

rf_tuning_results_b <- rf_tuning_results_b %>%
  arrange(validation_RMSE)

cat("\nRandom Forest tuning results for Part (b):\n")
print(rf_tuning_results_b)

best_rf_params_b <- rf_tuning_results_b[1, ]

cat("\nBest Random Forest parameters for Part (b):\n")
print(best_rf_params_b)




#===============================================================================
# b) 11. Final test forecasts using best hyperparameters
#===============================================================================

# After tuning is complete, fix the best hyperparameters.
#
# Do not continue tuning on the test set.
#
# For each test observation:
# Re-estimate the model using all data before that observation,
# then make a one-step-ahead forecast.

cat("\nStart final test forecast for Regression Tree in Part (b)...\n")

tree_test_forecast_b <- rolling_forecast_rpart_b(
  data = ml_cases_data_part_b,
  start_idx = val_end_b + 1,
  end_idx = n_total_b,
  predictor_cols = predictor_cols_part_b,
  cp = best_tree_params_b$cp,
  maxdepth = best_tree_params_b$maxdepth,
  minsplit = best_tree_params_b$minsplit
)

cat("\nStart final test forecast for Random Forest in Part (b)...\n")

rf_test_forecast_b <- rolling_forecast_ranger_b(
  data = ml_cases_data_part_b,
  start_idx = val_end_b + 1,
  end_idx = n_total_b,
  predictor_cols = predictor_cols_part_b,
  mtry = best_rf_params_b$mtry,
  min.node.size = best_rf_params_b$min.node.size,
  num.trees = best_rf_params_b$num.trees,
  seed = 2026,
  num.threads = n_threads_ranger_b
)


#===============================================================================
# b) 12. Compute test MSFE and RMSE for Part (b)
#===============================================================================

tree_test_msfe_b <- msfe_fun(actual = tree_test_forecast_b$actual,forecast = tree_test_forecast_b$forecast)
tree_test_rmse_b <- rmse_fun(actual = tree_test_forecast_b$actual,forecast = tree_test_forecast_b$forecast)

rf_test_msfe_b <- msfe_fun(actual = rf_test_forecast_b$actual,forecast = rf_test_forecast_b$forecast)
rf_test_rmse_b <- rmse_fun(actual = rf_test_forecast_b$actual,forecast = rf_test_forecast_b$forecast)


part_b_results <- data.frame(
  Model = c("Regression Tree", "Random Forest"),
  Predictors = c(
    "log cases lags 1-10 + covariate lags 1-10",
    "log cases lags 1-10 + covariate lags 1-10"
  ),
  Covariates = c(
    "temp_c, share_cases_60plus, share_cases_80plus, is_start_week",
    "temp_c, share_cases_60plus, share_cases_80plus, is_start_week"
  ),
  Test_MSFE = c(tree_test_msfe_b, rf_test_msfe_b),
  Test_RMSE = c(tree_test_rmse_b, rf_test_rmse_b)
)

# Display the results.
cat("\nFinal test results for Part (b):\n")
knitr::kable(part_b_results, digits = 6)


#===============================================================================
# b) 13. Combine actual values and forecasts into one dataframe
#===============================================================================

test_forecasts_part_b <- data.frame(
  date = tree_test_forecast_b$date,
  actual_log_cases = tree_test_forecast_b$actual,
  regression_tree_forecast = tree_test_forecast_b$forecast,
  random_forest_forecast = rf_test_forecast_b$forecast
)


#===============================================================================
# b) 14. Plot actual log cases and forecasts in test period
#===============================================================================

test_forecasts_long_b <- test_forecasts_part_b %>%
  tidyr::pivot_longer(
    cols = c(
      actual_log_cases,
      regression_tree_forecast,
      random_forest_forecast
    ),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = dplyr::recode(
      series,
      actual_log_cases = "Actual log cases",
      regression_tree_forecast = "Regression Tree forecast",
      random_forest_forecast = "Random Forest forecast"
    )
  )

# Generate the MSFE / RMSE text displayed in the plot.
performance_text_b <- sprintf(
  "Regression Tree: MSFE = %.4f, RMSE = %.4f\nRandom Forest: MSFE = %.4f, RMSE = %.4f",
  tree_test_msfe_b, tree_test_rmse_b,
  rf_test_msfe_b, rf_test_rmse_b
)

# Set the annotation position in the upper-left corner of the plot.
x_pos_b <- min(test_forecasts_long_b$date, na.rm = TRUE)
y_pos_b <- max(test_forecasts_long_b$value, na.rm = TRUE)

plot_part_b_forecasts <- ggplot(
  test_forecasts_long_b,
  aes(x = date, y = value, color = series)
) +
  geom_line(linewidth = 0.7) +
  
  # Add two lines of MSFE / RMSE results to the plot.
  annotate(
    "label",
    x = x_pos_b,
    y = y_pos_b,
    label = performance_text_b,
    hjust = 0,
    vjust = 1,
    size = 3.5,
    color = "black",
    fill = "white",
    label.size = 0.25
  ) +
  
  # Manually adjust the colors.
  scale_color_manual(
    values = c(
      "Actual log cases" = "gray80",
      "Regression Tree forecast" = "orange",
      "Random Forest forecast" = "#0072B2"
    )
  ) +
  
  labs(
    title = "One-step-ahead forecasts of log cases with covariates: Regression Tree vs Random Forest",
    subtitle = "Predictors: lags 1-10 of log cases and lagged covariates",
    x = "Date",
    y = "log(1 + daily cases)",
    color = "Series"
  ) +
  theme_minimal()

print(plot_part_b_forecasts)

ggsave(
  filename = file.path(folder_name, "2. Part_b_log_cases_forecasts_tree_rf_with_covariates.png"),
  plot = plot_part_b_forecasts,
  width = 10,
  height = 6,
  dpi = 300
)



#===============================================================================
# b) 15. Optional: compare Part (a) and Part (b), if Part (a) results exist
#===============================================================================

# If Part (a) has already been run earlier and the object part_a_results exists,
# this will automatically combine the results from Part (a) and Part (b) for easier comparison:
#
# a) only log cases lags
# b) log cases lags + covariate lags

if (exists("part_a_results")) {
  
  part_a_results_for_comparison <- part_a_results %>%
    mutate(Part = "Part (a): cases lags only") %>%
    select(Part, Model, Predictors, Test_MSFE, Test_RMSE)
  
  part_b_results_for_comparison <- part_b_results %>%
    mutate(Part = "Part (b): cases lags + covariate lags") %>%
    select(Part, Model, Predictors, Test_MSFE, Test_RMSE)
  
  part_a_b_comparison <- bind_rows(
    part_a_results_for_comparison,
    part_b_results_for_comparison
  )
  
  cat("\nComparison of Part (a) and Part (b):\n")
  print(part_a_b_comparison)
  
  knitr::kable(part_a_b_comparison, digits = 6)
  
}


















#===============================================================================
#-------------- 5. Variable Importance: Part (c)
#-------------- Global importance ranking for the forecasts in Part (b)
#-------------- Methods:
#-------------- 1. Grouped Permutation Feature Importance (PFI)
#-------------- 2. Grouped Leave One Feature Out (LOFO) Importance
#===============================================================================


#===============================================================================
# c) 1. Why global importance and why grouped variables?
#===============================================================================

# The assignment asks us to rank the importance of the variables, meaning the overall importance ranking of variables in the full forecasting problem.
# Therefore, global model-agnostic importance methods are used here.
#
# Local model-agnostic methods, such as LIME or Shapley values for a single observation,
# are mainly used to explain why the forecast for one specific date is high or low. They answer:
# "Why did the model make this prediction for this observation?"
#
# Parts c) and d) focus more on:
# "Which variables are most important for the model's forecasts overall?"
#
# Therefore, two global importance measures are selected here:
#
# 1. PFI:
#    Shuffle one group of variables in the test set. If MSFE rises noticeably after shuffling, the model relies on that group.
#
# 2. LOFO:
#    Completely remove one group of variables from the training and test data, and then retrain the model.
#    If MSFE rises noticeably after removal, that group provides important predictive information.
#
# Why use grouped importance instead of comparing y_lag1, y_lag2, ... individually?
#
# Because each original variable in this assignment has 10 lags, and these lags are usually highly correlated.
# If importance is computed lag by lag, importance may be spread across multiple lags and become less intuitive to interpret.
# Here, the 10 lags of the same original variable are placed in one group and processed together.
#
# The final comparison is among:
# log_cases, temp_c, share_cases_60plus, share_cases_80plus, is_start_week




#===============================================================================
# c) 2. Define grouped variables for Part (b)
#===============================================================================

importance_groups_part_c <- list(
  log_cases = paste0("y_lag", 1:max_lag),
  temp_c = paste0("temp_c_lag", 1:max_lag),
  share_cases_60plus = paste0("share_cases_60plus_lag", 1:max_lag),
  share_cases_80plus = paste0("share_cases_80plus_lag", 1:max_lag),
  is_start_week = paste0("is_start_week_lag", 1:max_lag)
)

# Because Part (c) is based on Part (b), check whether the columns in each variable group really exist in the Part (b) modeling data.
# If an error occurs here, it usually means the column names in the lagged dataset changed earlier.
missing_importance_cols <- unlist(importance_groups_part_c) %>%
  setdiff(predictor_cols_part_b)

if (length(missing_importance_cols) > 0) {
  stop(
    "Some grouped importance columns are missing from predictor_cols_part_b: ",
    paste(missing_importance_cols, collapse = ", ")
  )
}




#===============================================================================
# c) 3. Refit final Part (b) models for interpretation
#===============================================================================

# The formal test evaluation in Part (b) is rolling one-step-ahead:
# Each test point re-estimates the model.
#
# However, Part (c) performs variable-importance ranking. To make PFI and LOFO clearer and more stable,
# a final explanation model is trained here following common explanation-method practice:
#
# training data:
#   All data before the test set in Part (b), that is, rows 1 to val_end_b.
#
# test data:
#   The same test set as in Part (b), that is, test_idx_b.
#
# This does not change the forecasting results in Part (b); it is only for explaining the model on the same held-out test set.

train_data_importance_b <- ml_cases_data_part_b[1:val_end_b, , drop = FALSE]
test_data_importance_b <- ml_cases_data_part_b[test_idx_b, , drop = FALSE]

# To avoid best_tree_params_b$cp becoming character because of earlier display formatting,
# explicitly convert it to numeric here. Even if it is already numeric, this step does not change the value.
best_tree_cp_b <- as.numeric(best_tree_params_b$cp)
best_tree_maxdepth_b <- as.integer(best_tree_params_b$maxdepth)
best_tree_minsplit_b <- as.integer(best_tree_params_b$minsplit)

best_rf_mtry_b <- as.integer(best_rf_params_b$mtry)
best_rf_min_node_b <- as.integer(best_rf_params_b$min.node.size)
best_rf_num_trees_b <- as.integer(best_rf_params_b$num.trees)





#===============================================================================
# c) 4. Helper functions for fitting and prediction
#===============================================================================

fit_tree_part_c <- function(train_data, predictor_cols) {
  # This function trains a regression tree using the fixed best hyperparameters from Part (b).
  # train_data must include target and predictor_cols.
  rpart(
    formula = target ~ .,
    data = train_data[, c("target", predictor_cols), drop = FALSE],
    method = "anova",
    control = rpart.control(
      cp = best_tree_cp_b,
      maxdepth = best_tree_maxdepth_b,
      minsplit = best_tree_minsplit_b,
      xval = 0
    )
  )
}

fit_rf_part_c <- function(train_data, predictor_cols) {
  # This function trains a random forest using the fixed best hyperparameters from Part (b).
  # If LOFO removes some variables, the remaining number of variables may be smaller than the original mtry,
  # so min() is used here to prevent mtry from exceeding the current number of predictors.
  ranger(
    formula = target ~ .,
    data = train_data[, c("target", predictor_cols), drop = FALSE],
    num.trees = best_rf_num_trees_b,
    mtry = min(best_rf_mtry_b, length(predictor_cols)),
    min.node.size = best_rf_min_node_b,
    seed = 2026,
    num.threads = n_threads_ranger_b
  )
}

predict_tree_part_c <- function(model, new_x) {
  as.numeric(predict(model, newdata = new_x))
}

predict_rf_part_c <- function(model, new_x) {
  as.numeric(predict(model, data = new_x)$predictions)
}




#===============================================================================
# c) 5. Fit baseline explanation models and compute baseline MSFE
#===============================================================================

tree_model_importance_b <- fit_tree_part_c(
  train_data = train_data_importance_b,
  predictor_cols = predictor_cols_part_b
)

rf_model_importance_b <- fit_rf_part_c(
  train_data = train_data_importance_b,
  predictor_cols = predictor_cols_part_b
)

tree_baseline_pred_b <- predict_tree_part_c(
  model = tree_model_importance_b,
  new_x = test_data_importance_b[, predictor_cols_part_b, drop = FALSE]
)

rf_baseline_pred_b <- predict_rf_part_c(
  model = rf_model_importance_b,
  new_x = test_data_importance_b[, predictor_cols_part_b, drop = FALSE]
)

tree_baseline_msfe_b <- msfe_fun(
  actual = test_data_importance_b$target,
  forecast = tree_baseline_pred_b
)

rf_baseline_msfe_b <- msfe_fun(
  actual = test_data_importance_b$target,
  forecast = rf_baseline_pred_b
)

cat("\nPart (c) baseline MSFE for final explanation models:\n")
cat("Regression Tree baseline MSFE:", tree_baseline_msfe_b, "\n")
cat("Random Forest baseline MSFE:", rf_baseline_msfe_b, "\n")




#===============================================================================
# c) 6. Grouped Permutation Feature Importance (PFI)
#===============================================================================

compute_grouped_pfi <- function(model,
                                model_name,
                                test_data,
                                predictor_cols,
                                variable_groups,
                                predict_fun,
                                baseline_msfe,
                                n_repeats = 30,
                                seed = 2026) {
  
  # Core idea of PFI:
  # 1. First calculate the baseline MSFE on the original test set.
  # 2. For one variable group, such as the 10 lags of temp_c, shuffle their row order together.
  # 3. Use the same trained model to predict again.
  # 4. If MSFE increases a lot, the model relies on this variable group.
  #
  # Note:
  # The 10 lags within the same variable group use the same permutation index.
  # This preserves the relative structure among lags within the group and avoids shuffling lag1 and lag2 separately.
  
  pfi_results <- data.frame()
  actual_y <- test_data$target
  base_x <- test_data[, predictor_cols, drop = FALSE]
  
  for (group_name in names(variable_groups)) {
    
    group_cols <- variable_groups[[group_name]]
    repeated_msfe <- numeric(n_repeats)
    
    for (r in 1:n_repeats) {
      
      set.seed(seed + r)
      
      permuted_x <- base_x
      permutation_index <- sample(1:nrow(permuted_x), size = nrow(permuted_x), replace = FALSE)
      
      # Shuffle all lags in the same variable group together using the same row order.
      permuted_x[, group_cols] <- permuted_x[permutation_index, group_cols, drop = FALSE]
      
      permuted_pred <- predict_fun(
        model = model,
        new_x = permuted_x
      )
      
      repeated_msfe[r] <- msfe_fun(
        actual = actual_y,
        forecast = permuted_pred
      )
    }
    
    pfi_results <- rbind(
      pfi_results,
      data.frame(
        Model = model_name,
        Variable_Group = group_name,
        Baseline_MSFE = baseline_msfe,
        PFI_MSFE_Mean = mean(repeated_msfe, na.rm = TRUE),
        PFI_MSFE_SD = sd(repeated_msfe, na.rm = TRUE),
        PFI_diff = mean(repeated_msfe - baseline_msfe, na.rm = TRUE),
        PFI_ratio = mean(repeated_msfe / baseline_msfe, na.rm = TRUE),
        N_Repeats = n_repeats
      )
    )
  }
  
  pfi_results <- pfi_results %>%
    arrange(desc(PFI_diff))
  
  return(pfi_results)
}


pfi_tree_b <- compute_grouped_pfi(
  model = tree_model_importance_b,
  model_name = "Regression Tree",
  test_data = test_data_importance_b,
  predictor_cols = predictor_cols_part_b,
  variable_groups = importance_groups_part_c,
  predict_fun = predict_tree_part_c,
  baseline_msfe = tree_baseline_msfe_b,
  n_repeats = 30,
  seed = 2026
)

pfi_rf_b <- compute_grouped_pfi(
  model = rf_model_importance_b,
  model_name = "Random Forest",
  test_data = test_data_importance_b,
  predictor_cols = predictor_cols_part_b,
  variable_groups = importance_groups_part_c,
  predict_fun = predict_rf_part_c,
  baseline_msfe = rf_baseline_msfe_b,
  n_repeats = 30,
  seed = 2026
)

cat("\nGrouped PFI results for Regression Tree:\n")
print(pfi_tree_b)

cat("\nGrouped PFI results for Random Forest:\n")
print(pfi_rf_b)


#===============================================================================
# c) 7. Grouped Leave One Feature Out (LOFO) Importance
#===============================================================================

compute_grouped_lofo <- function(model_name,
                                 train_data,
                                 test_data,
                                 predictor_cols,
                                 variable_groups,
                                 fit_fun,
                                 predict_fun,
                                 baseline_msfe) {
  
  # Core idea of LOFO:
  # 1. Remove one variable group from the training and test data.
  # 2. Retrain the model using the remaining variables.
  # 3. Calculate MSFE on the same test set.
  # 4. If MSFE increases a lot after removal, this variable group is important.
  #
  # Hyperparameter tuning is not repeated here.
  # Reason:
  # - Retuning would require a very large amount of computation.
  # - Fixing the best hyperparameters from Part (b) makes it easier to stay consistent with the final model setup in Part (b).
  
  lofo_results <- data.frame()
  actual_y <- test_data$target
  
  for (group_name in names(variable_groups)) {
    
    group_cols <- variable_groups[[group_name]]
    remaining_cols <- setdiff(predictor_cols, group_cols)
    
    lofo_model <- fit_fun(
      train_data = train_data,
      predictor_cols = remaining_cols
    )
    
    lofo_pred <- predict_fun(
      model = lofo_model,
      new_x = test_data[, remaining_cols, drop = FALSE]
    )
    
    lofo_msfe <- msfe_fun(
      actual = actual_y,
      forecast = lofo_pred
    )
    
    lofo_results <- rbind(
      lofo_results,
      data.frame(
        Model = model_name,
        Variable_Group = group_name,
        Baseline_MSFE = baseline_msfe,
        LOFO_MSFE = lofo_msfe,
        LOFO_diff = lofo_msfe - baseline_msfe,
        LOFO_ratio = lofo_msfe / baseline_msfe
      )
    )
  }
  
  lofo_results <- lofo_results %>%
    arrange(desc(LOFO_diff))
  
  return(lofo_results)
}


lofo_tree_b <- compute_grouped_lofo(
  model_name = "Regression Tree",
  train_data = train_data_importance_b,
  test_data = test_data_importance_b,
  predictor_cols = predictor_cols_part_b,
  variable_groups = importance_groups_part_c,
  fit_fun = fit_tree_part_c,
  predict_fun = predict_tree_part_c,
  baseline_msfe = tree_baseline_msfe_b
)

lofo_rf_b <- compute_grouped_lofo(
  model_name = "Random Forest",
  train_data = train_data_importance_b,
  test_data = test_data_importance_b,
  predictor_cols = predictor_cols_part_b,
  variable_groups = importance_groups_part_c,
  fit_fun = fit_rf_part_c,
  predict_fun = predict_rf_part_c,
  baseline_msfe = rf_baseline_msfe_b
)

cat("\nGrouped LOFO results for Regression Tree:\n")
print(lofo_tree_b)

cat("\nGrouped LOFO results for Random Forest:\n")
print(lofo_rf_b)


#===============================================================================
# c) 8. Combine PFI and LOFO results
#===============================================================================

pfi_part_c_results <- bind_rows(pfi_tree_b, pfi_rf_b) %>%
  transmute(
    Model,
    Importance_Method = "PFI",
    Variable_Group,
    Baseline_MSFE,
    Importance_MSFE = PFI_MSFE_Mean,
    Importance_Diff = PFI_diff,
    Importance_Ratio = PFI_ratio,
    Importance_SD = PFI_MSFE_SD
  )

lofo_part_c_results <- bind_rows(lofo_tree_b, lofo_rf_b) %>%
  transmute(
    Model,
    Importance_Method = "LOFO",
    Variable_Group,
    Baseline_MSFE,
    Importance_MSFE = LOFO_MSFE,
    Importance_Diff = LOFO_diff,
    Importance_Ratio = LOFO_ratio,
    Importance_SD = NA_real_
  )

part_c_importance_results <- bind_rows(
  pfi_part_c_results,
  lofo_part_c_results
) %>%
  arrange(Model, Importance_Method, desc(Importance_Diff))

cat("\nCombined Part (c) importance results:\n")
print(part_c_importance_results)

knitr::kable(part_c_importance_results, digits = 6)


#===============================================================================
# c) 9. Plot grouped importance results
#===============================================================================

# To make the variable order in the plot more stable, sort by the average importance across all models and methods.
# Variable groups with larger average Importance_Diff appear closer to the top of the plot.
variable_order_part_c <- part_c_importance_results %>%
  group_by(Variable_Group) %>%
  summarise(
    Mean_Importance_Diff = mean(Importance_Diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Mean_Importance_Diff) %>%
  pull(Variable_Group)

plot_part_c_data <- part_c_importance_results %>%
  mutate(
    Variable_Group = factor(Variable_Group, levels = variable_order_part_c)
  )

plot_part_c_importance <- ggplot(
  plot_part_c_data,
  aes(x = Variable_Group, y = Importance_Diff, fill = Importance_Method)
) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_grid(Model ~ Importance_Method, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "PFI" = "#0072B2",
      "LOFO" = "orange"
    )
  ) +
  labs(
    title = "Part (c): Grouped variable importance for Part (b) models",
    subtitle = "Importance is measured by the increase in test MSFE on log(1 + daily cases)",
    x = "Variable group",
    y = "Increase in MSFE",
    fill = "Importance method"
  ) +
  theme_minimal()

print(plot_part_c_importance)

ggsave(
  filename = file.path(folder_name, "3.Part_c_grouped_pfi_lofo_importance.png"),
  plot = plot_part_c_importance,
  width = 10,
  height = 7,
  dpi = 300
)





















#===============================================================================
#-------------- 6. Discussion: Part (d)
#-------------- Grouped-level discussion and lag-level Top 5 importance
#===============================================================================


#===============================================================================
# d) 1. Build grouped-level summary from Part (c)
#===============================================================================

# Part (c) has already produced grouped variable importance:
# log_cases, temp_c, share_cases_60plus, share_cases_80plus, is_start_week
#
# Note:
# These variable groups are not single variables; each is the full set of lags 1 to 10 for one original variable.
# For example, log_cases represents the overall importance of y_lag1, y_lag2, ..., y_lag10.
#
# Part (d) first summarizes the grouped-level ranking, then further expands it to individual lagged predictors.

part_d_importance_summary <- part_c_importance_results %>%
  group_by(Model, Importance_Method) %>%
  arrange(desc(Importance_Diff), .by_group = TRUE) %>%
  mutate(
    Rank = row_number()
  ) %>%
  ungroup() %>%
  select(
    Model,
    Importance_Method,
    Rank,
    Variable_Group,
    Importance_Diff,
    Importance_Ratio,
    Baseline_MSFE,
    Importance_MSFE
  )

cat("\nPart (d) grouped-level importance summary:\n")
print(part_d_importance_summary)

knitr::kable(part_d_importance_summary, digits = 6)


#===============================================================================
# d) 2. Helper function: parse one lagged predictor name
#===============================================================================

parse_lagged_predictor_part_d <- function(predictor_name) {
  
  # This function splits the predictor name from Part (b) into two parts:
  #
  # 1. Original_Variable:
  #    Original variable name, such as log_cases, temp_c, or is_start_week.
  #
  # 2. Lag:
  #    Lag order, such as 1, 2, ..., 10.
  #
  # Examples:
  # y_lag3                  -> Original_Variable = log_cases, Lag = 3
  # temp_c_lag7             -> Original_Variable = temp_c, Lag = 7
  # share_cases_80plus_lag2 -> Original_Variable = share_cases_80plus, Lag = 2
  
  if (grepl("^y_lag[0-9]+$", predictor_name)) {
    original_variable <- "log_cases"
    lag_number <- as.integer(gsub("^y_lag", "", predictor_name))
  } else {
    original_variable <- gsub("_lag[0-9]+$", "", predictor_name)
    lag_number <- as.integer(gsub("^.*_lag", "", predictor_name))
  }
  
  data.frame(
    Lagged_Predictor = predictor_name,
    Original_Variable = original_variable,
    Lag = lag_number
  )
}


#===============================================================================
# d) 3. Lag-level Permutation Feature Importance
#===============================================================================

compute_lag_level_pfi <- function(model,
                                  model_name,
                                  test_data,
                                  predictor_cols,
                                  predict_fun,
                                  baseline_msfe,
                                  n_repeats = 30,
                                  seed = 3026) {
  
  # Here, all lagged predictors are expanded and PFI is computed one by one.
  #
  # Different from the grouped PFI in Part (c):
  # - Part (c): shuffle the 10 lags of one variable at a time, answering "Which original variable is most important overall?"
  # - Part (d): shuffle only one specific lag at a time, answering "Which specific lagged term is most important?"
  #
  # Because different lags are usually highly correlated, lag-level PFI may be less stable than grouped PFI.
  # Therefore, the main conclusion in Part (d) is still based primarily on grouped importance,
  # and the lag-level Top 5 is mainly used as supplementary display.
  
  lag_level_results <- data.frame()
  actual_y <- test_data$target
  base_x <- test_data[, predictor_cols, drop = FALSE]
  
  for (predictor_name in predictor_cols) {
    
    repeated_msfe <- numeric(n_repeats)
    
    for (r in 1:n_repeats) {
      
      set.seed(seed + r)
      
      permuted_x <- base_x
      permutation_index <- sample(1:nrow(permuted_x), size = nrow(permuted_x), replace = FALSE)
      
      # Here, only one specific predictor is shuffled at a time, for example y_lag1 or temp_c_lag3.
      permuted_x[, predictor_name] <- permuted_x[permutation_index, predictor_name]
      
      permuted_pred <- predict_fun(
        model = model,
        new_x = permuted_x
      )
      
      repeated_msfe[r] <- msfe_fun(
        actual = actual_y,
        forecast = permuted_pred
      )
    }
    
    predictor_info <- parse_lagged_predictor_part_d(predictor_name)
    
    lag_level_results <- rbind(
      lag_level_results,
      data.frame(
        Model = model_name,
        Lagged_Predictor = predictor_info$Lagged_Predictor,
        Original_Variable = predictor_info$Original_Variable,
        Lag = predictor_info$Lag,
        Baseline_MSFE = baseline_msfe,
        PFI_MSFE_Mean = mean(repeated_msfe, na.rm = TRUE),
        PFI_diff = mean(repeated_msfe - baseline_msfe, na.rm = TRUE),
        PFI_ratio = mean(repeated_msfe / baseline_msfe, na.rm = TRUE),
        PFI_SD = sd(repeated_msfe, na.rm = TRUE),
        N_Repeats = n_repeats
      )
    )
  }
  
  lag_level_results <- lag_level_results %>%
    arrange(Model, desc(PFI_diff))
  
  return(lag_level_results)
}


part_d_lag_level_pfi_tree <- compute_lag_level_pfi(
  model = tree_model_importance_b,
  model_name = "Regression Tree",
  test_data = test_data_importance_b,
  predictor_cols = predictor_cols_part_b,
  predict_fun = predict_tree_part_c,
  baseline_msfe = tree_baseline_msfe_b,
  n_repeats = 30,
  seed = 2026
)

part_d_lag_level_pfi_rf <- compute_lag_level_pfi(
  model = rf_model_importance_b,
  model_name = "Random Forest",
  test_data = test_data_importance_b,
  predictor_cols = predictor_cols_part_b,
  predict_fun = predict_rf_part_c,
  baseline_msfe = rf_baseline_msfe_b,
  n_repeats = 30,
  seed = 2026
)

part_d_lag_level_pfi_results <- bind_rows(
  part_d_lag_level_pfi_tree,
  part_d_lag_level_pfi_rf
) %>%
  arrange(Model, desc(PFI_diff))

# Simple check: each of the two models should have length(predictor_cols_part_b) lag-level PFI results.
# If the row count is incorrect, it means that the PFI calculation for some model or predictor was skipped.
expected_lag_level_rows_part_d <- 2 * length(predictor_cols_part_b)

if (nrow(part_d_lag_level_pfi_results) != expected_lag_level_rows_part_d) {
  warning(
    "Unexpected number of lag-level PFI rows. Expected ",
    expected_lag_level_rows_part_d,
    ", but got ",
    nrow(part_d_lag_level_pfi_results),
    "."
  )
}



#===============================================================================
# d) 4. Extract Top 5 lagged predictors for each model
#===============================================================================

part_d_top5_lag_summary <- part_d_lag_level_pfi_results %>%
  group_by(Model) %>%
  arrange(desc(PFI_diff), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  mutate(
    Rank = row_number(),
    Lagged_Predictor_Label = paste0(Lagged_Predictor, " (", Original_Variable, ", lag ", Lag, ")")
  ) %>%
  ungroup() %>%
  select(
    Model,
    Rank,
    Lagged_Predictor,
    Original_Variable,
    Lag,
    PFI_diff,
    PFI_ratio,
    Baseline_MSFE,
    PFI_MSFE_Mean,
    PFI_SD
  )

cat("\nPart (d) Top 5 lag-level PFI summary:\n")
print(part_d_top5_lag_summary)



#===============================================================================
# d) 5. Plot Top 5 lag-level importance
#===============================================================================

plot_part_d_top5_data <- part_d_top5_lag_summary %>%
  mutate(
    Lagged_Predictor_Label = paste0(Lagged_Predictor, " (", Original_Variable, ", lag ", Lag, ")"),
    Lagged_Predictor_Label = reorder(Lagged_Predictor_Label, PFI_diff)
  )

plot_part_d_top5_lag_importance <- ggplot(
  plot_part_d_top5_data,
  aes(x = Lagged_Predictor_Label, y = PFI_diff, fill = Original_Variable)
) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(~ Model, scales = "free_y") +
  labs(
    title = "Part (d): Top 5 lag-level predictors by PFI",
    subtitle = "Each bar is one specific lagged predictor; importance is the increase in test MSFE",
    x = "Lagged predictor",
    y = "Increase in MSFE",
    fill = "Original variable"
  ) +
  theme_minimal()

print(plot_part_d_top5_lag_importance)

ggsave(
  filename = file.path(folder_name, "4.Part_d_top5_lag_level_importance.png"),
  plot = plot_part_d_top5_lag_importance,
  width = 10,
  height = 6,
  dpi = 300
)



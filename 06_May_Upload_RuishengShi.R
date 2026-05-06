library(dplyr)
library(knitr) #latex
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(forecast)   # for Acf(), Pacf(), and later Arima()
library(zoo)        # for as.Date / time series helpers
library(patchwork)  # for combining ggplots side by side
library(tseries)    # for adf.test() and kpss.test()，Jarque-Bera Test
library(lmtest)
library(future.apply)
library(jsonlite)

##==============================================================================
##=============================0. Some Preparation======================================
##==============================================================================
#data_infektionen is downloaded from https://github.com/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland
#immunizations datasets are downloaded from https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/main/Deutschland_Impfquoten_COVID-19.csv

# Identify Working path
working_path <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# create a folder to save plots
folder_name <- "RuishengShi_Plots"
if (!dir.exists(folder_name)) {dir.create(folder_name)}




##==============================================================================
##=============================1. Load Data=====================================
##==============================================================================

# Read infektionen data
# laptop version file_path
data_infektionen_file_path_laptop = "D:/one_drive/OneDrive/Desktop/课程文件/14. Case study/assignment0/Aktuell_Deutschland_SarsCov2_Infektionen.csv"
data_infektionen <- read_csv(data_infektionen_file_path_laptop)

# desktop version file_path
#data_infektionen_file_path_desktop = "C:/Users/RAY/OneDrive/Desktop/课程文件/14. Case study/assignment0/Aktuell_Deutschland_SarsCov2_Infektionen.csv"
#data_infektionen <- read_csv(data_infektionen_file_path_desktop)


# data cleaning
# 1. set date range
analysis_start <- as.Date("2020-04-01")
analysis_end   <- as.Date("2026-03-31")
data_infektionen <- data_infektionen %>%
  filter(Meldedatum >= analysis_start, Meldedatum <= analysis_end)

# 2. extract state code
data_infektionen <- data_infektionen %>%
  mutate(BundeslandId_Impfort = IdLandkreis %/% 1000, .after = IdLandkreis)

# Check the value distribution of each column separately
table(data_infektionen$NeuerFall)
table(data_infektionen$NeuerTodesfall)
table(data_infektionen$NeuGenesen)






##==============================================================================
##=======================2. Compute Cases and Death data========================
## Based on Meldedatum, calculate infection cases and death cases (recovered cases are not calculated for now, as they are not required by the assignment)
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
  filter(NeuerTodesfall %in% c(0L, 1L)) %>%                        # Filter codes 0 and 1, excluding -9 and -1
  group_by(Meldedatum) %>%                                         # Group and sum by day to get daily death data
  summarise(deaths = sum(AnzahlTodesfall), .groups = "drop") %>%   # Sum up all corresponding AnzahlTodesfall, .groups = "drop" removes grouping
  arrange(Meldedatum)                                              # Sort the table in chronological order (starting from early 2020)




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



# ======================plots of raw datas====================================== 

# Time Series Plots
p1 <- ggplot(daily_cases, aes(x = Meldedatum, y = new_cases)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(title = "Daily Cases")

p2 <- ggplot(daily_deaths, aes(x = Meldedatum, y = deaths)) +
  geom_line(color = "firebrick") +
  theme_minimal() +
  labs(title = "Daily Deaths")

p1 / p2
TS_plot_raw <- p1 / p2

# save plots
ggsave(
  filename = file.path(folder_name, "1. Daily_Cases_and_Deaths.png"), 
  plot = TS_plot_raw,
  width = 10,     # It is recommended to set width and height to prevent image distortion
  height = 8,
  dpi = 300       # Set resolution to make the plot clearer for the teacher to view
)



# ACF and PACF
plot_acf_pacf <- function() {
  par(mfrow = c(2, 2), mar = c(3, 4, 3, 2))
  acf(daily_cases$new_cases, main = "ACF: Daily Cases")
  pacf(daily_cases$new_cases, main = "PACF: Daily Cases")
  acf(daily_deaths$deaths, main = "ACF: Daily Deaths")
  pacf(daily_deaths$deaths, main = "PACF: Daily Deaths")
}

plot_acf_pacf()

png(filename = file.path(folder_name, "2. ACF_PACF_Combined.png"), 
    width = 1800, height = 1200, res = 120)
plot_acf_pacf()
dev.off()
par(mfrow = c(1, 1))
# Analyze the ACF and PACF plots in the report (extremely slow decay, typical of: 1. non-stationary characteristics, 2. strong seasonality)


# # Use STL decomposition to show TS components
# cases_msts <- msts(daily_cases$new_cases, seasonal.periods = c(7, 365.25))
# cases_msts_decomposition <- mstl(cases_msts)
# plot(cases_msts_decomposition) 
# 
# cases_msts <- msts(daily_deaths$deaths, seasonal.periods = c(7, 365.25))
# cases_msts_decomposition <- mstl(cases_msts)
# plot(cases_msts_decomposition) 




# =============Stationary test: ADF-test and KPSS-test==========================

check_stationarity <- function(series, name) {
  cat("\n==============================================\n")
  cat("  Stationarity Analysis for:", name, "\n")
  cat("==============================================\n")
  
  # 1. ADF Test (Augmented Dickey-Fuller)
  # H0: Series is non-stationary
  cat("\n[1] Augmented Dickey-Fuller Test:\n")
  print(adf.test(series))
  
  # 2. PP Test (Phillips-Perron)
  # H0: Series is non-stationary (more robust to higher-order serial correlation)
  cat("\n[2] Phillips-Perron Unit Root Test:\n")
  print(pp.test(series))
  
  # 3. KPSS Test (Kwiatkowski-Phillips-Schmidt-Shin)
  # H0: Series is stationary (logic is opposite to the previous two)
  cat("\n[3] KPSS Unit Root Test:\n")
  print(kpss.test(series))
}

check_stationarity(daily_cases$new_cases, "Daily Cases")  
check_stationarity(daily_deaths$deaths, "Daily Deaths")
### You can explain the principles of ADF, PP, and KPSS in the report
### ADF and PP results show stationarity; KPSS result shows non-stationarity, leading to contradictory conclusions. Data processing is needed before fitting an AR model


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



# Time Series plots of processed datas.
p1 <- data.frame(d = tail(daily_cases$Meldedatum, -8), v = cases_processed) %>% 
  ggplot(aes(d, v)) + geom_line(color = "steelblue") + theme_minimal() + labs(title = "Processed Cases", x = NULL, y = NULL)

p2 <- data.frame(d = tail(daily_deaths$Meldedatum, -8), v = deaths_processed) %>% 
  ggplot(aes(d, v)) + geom_line(color = "firebrick") + theme_minimal() + labs(title = "Processed Deaths", x = NULL, y = NULL)

p1 / p2 #seems much more stationary than before
TS_plot_precessed <- p1 / p2

# save plots
ggsave(
  filename = file.path(folder_name, "3. Processed_Daily_Cases_and_Deaths.png"), 
  plot = TS_plot_precessed,
  width = 10,     # It is recommended to set width and height to prevent image distortion
  height = 8,
  dpi = 800       # Set resolution to make the plot clearer for the teacher to view
)



## ACF/PACF plots
plot_acf_pacf_processed <- function() {
  par(mfrow = c(2, 2), mar = c(3, 4, 3, 2))
  acf(cases_processed, main = "ACF: Precessed Daily Cases")
  pacf(cases_processed, main = "PACF: Precessed Daily Cases")
  acf(deaths_processed, main = "ACF: Precessed Daily Deaths")
  pacf(deaths_processed, main = "PACF: Precessed Daily Deaths")
}

plot_acf_pacf_processed()

png(filename = file.path(folder_name, "4. Processed_ACF_PACF_Combined.png"), 
    width = 1800, height = 1200, res = 120)
plot_acf_pacf_processed()
dev.off()
par(mfrow = c(1, 1))
# ACF decays rapidly into the confidence interval after the first few lags, indicating it is essentially stationary.
# ACF has a negative spike at P=7, then decays rapidly (cuts off), while PACF decays alternately at P=7, 14, 21 (tails off), indicating seasonality.
# ✅ Can use SMA(0,1,1)_7 model on the log-transformed data



# Stationary test
check_stationarity(cases_processed, "Processed Daily Cases")   
check_stationarity(deaths_processed, "Processed Daily Deaths") 
# Both TS are stationary and can be forecasted using an AR model. Next, select the AR order P using AIC, BIC, and HQIC, then fit the AR model with the full sample, and finally use the Ljung-Box test to check if the residuals are white noise.


#===============================================================================
#---------4. Select AR order
#===============================================================================
# Define a function to automatically loop and select the AR(p) order
get_ar_criteria <- function(series, max_p = 15) {
  # Create a result table
  criteria_table <- data.frame(
    p = 1:max_p, 
    AIC = NA, 
    BIC = NA, 
    HQIC = NA
  )
  
  n <- length(series)
  
  for (p in 1:max_p) {
    # Fit an AR(p) model to the processed stationary series
    fit <- Arima(series, order = c(p, 0, 0), include.mean = TRUE)
    
    # Extract AIC and BIC
    criteria_table$AIC[p] <- AIC(fit)
    criteria_table$BIC[p] <- BIC(fit)
    
    # Calculate HQIC (Hannan-Quinn Information Criterion)
    # Formula: -2*loglik + 2*k*log(log(n))
    k <- length(fit$coef) # Number of parameters
    criteria_table$HQIC[p] <- -2 * fit$loglik + 2 * k * log(log(n))
  }
  
  return(criteria_table)
}

# Run the function (will cost a while)
results_cases <- get_ar_criteria(cases_processed)
results_deaths <- get_ar_criteria(deaths_processed)

# View results
print("Information Criteria for Cases:")
print(results_cases)
print(results_deaths)


# Plot to show results
plot_info_criteria <- function(data, title_suffix = "") {
  # 1. Convert to long format data
  data_long <- data %>%
    pivot_longer(cols = c(AIC, BIC, HQIC), 
                 names_to = "Criterion", 
                 values_to = "Value")
  
  # 2. Generate and return the chart
  p_out <- ggplot(data_long, aes(x = p, y = Value, color = Criterion)) +
    geom_line(linewidth = 1) +
    geom_point() +
    facet_wrap(~Criterion, ncol = 1, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Information Criteria:", title_suffix),
         x = "Order p", 
         y = "Score") +
    theme(legend.position = "none")
  
  return(p_out)
}


p1 <- plot_info_criteria(results_cases, "Cases")
p2 <- plot_info_criteria(results_deaths, "Deaths")

combined_IC_plot <- p1 / p2
print(combined_IC_plot)

ggsave(
  filename = file.path(folder_name, "5. Combined_AIC_BIC_HQIC.png"), 
  plot = combined_IC_plot,
  width = 10, 
  height = 12, # Increase height to accommodate two sets of subplots
  dpi = 800
)


# Noticed that IC continues to decrease, unable to select the optimal order
# On one hand, the PACF plot shows remaining periodicity at 7, 14, 21, 28, causing IC to select a very high order
# On the other hand, when differencing the time series, over-differencing introduces an MA structure. If the original series is not strongly non-stationary, differencing introduces an MA structure like εt - εt-1
# According to Wold's Decomposition Theorem, MA can be approximated by AR(∞), hence high-order AR performs better
# But we cannot actually choose p=∞ or a very high p, otherwise it will lead to overfitting (try forecasting?), resulting in poor out-of-sample prediction (find literature)
# Therefore, I choose p=2 or p=7, because p=2 is concise and aligns with Occam's razor; while p=7 contains information of a full cycle without having too high a lag order that causes overfitting
# Can introduce seasonal models like SAR or SMA later to test



#---------Prove MA Structural-------------
# if MA coefficient is significant
MA_proof_model <- arima(cases_processed, 
                        order = c(1, 0, 1),  # AR(1)+MA(1)
                        seasonal = list(order = c(0, 0, 1), period = 7), # A seasonal MA of period 7
                        include.mean = TRUE)

cat("--- Provement MA: If the coefficient is significant ---\n")
print(coeftest(MA_proof_model)) # MA and SMA are significant



# if AIC/BIC decrease when including MA term
AR14_model <- arima(cases_processed, order = c(14, 0, 0), include.mean = TRUE)
cat("\n--- Empirical Evidence 2: Information Criterion (IC) Comparison ---\n")
cat("Parsimonious model including MA term (number of parameters = 3) AIC: ", AIC(MA_proof_model), "\n")
cat("Large pure AR(50) model (number of parameters = 14) AIC: ", AIC(AR14_model), "\n")
cat("Parsimonious model including MA term (number of parameters = 3) BIC: ", BIC(MA_proof_model), "\n")
cat("Large pure AR(50) model (number of parameters = 14) BIC: ", BIC(AR14_model), "\n")
# IC decrease significantly after adding MA term
# ➡ We have proved there exists MA structurals




#===============================================================================
#---------5. But as requested in the project ,we need to fit pure AR(p)
#===============================================================================
# Train an AR(2) and an AR(7) respectively
# Fit AR and show results
cases_ar2 <- Arima(cases_processed, order = c(2, 0, 0), include.mean = TRUE)
cases_ar7 <- Arima(cases_processed, order = c(7, 0, 0), include.mean = TRUE)
cases_ar14 <- Arima(cases_processed, order = c(14, 0, 0), include.mean = TRUE)
summary(cases_ar2)
summary(cases_ar7)  # better than AR(2)
summary(cases_ar14) # better than AR(7), but will not be selected to prevent overfitting


deaths_ar2 <- Arima(deaths_processed, order = c(2, 0, 0), include.mean = TRUE)
deaths_ar7 <- Arima(deaths_processed, order = c(7, 0, 0), include.mean = TRUE)
deaths_ar14 <- Arima(deaths_processed, order = c(14, 0, 0), include.mean = TRUE)
summary(deaths_ar2)
summary(deaths_ar7)  # better than AR(2)
summary(deaths_ar14) # better than AR(7), but will not be selected to prevent overfitting

#==================== backshift representation of A R===========================

extract_backshift <- function(fit_model) {
  # Extract all coefficients
  coefs <- fit_model$coef
  
  # Extract AR coefficients (starting with ar)
  ar_coefs <- coefs[grep("^ar", names(coefs))]
  
  # Find the constant term (could be mean or intercept)
  intercept_idx <- which(names(coefs) %in% c("mean", "intercept", "drift"))
  if (length(intercept_idx) > 0) {
    mu <- coefs[intercept_idx]
  } else {
    mu <- 0
  }
  
  # The mean reported by Arima is the mean of the series (mu)
  # The constant c on the right side of the backshift form = mu * (1 - sum(phi_i))
  c_val <- mu * (1 - sum(ar_coefs))
  
  # Build the polynomial part
  poly_terms <- paste0(ifelse(ar_coefs > 0, " - ", " + "), 
                       abs(round(ar_coefs, 4)), "B^", 1:length(ar_coefs))
  formula_poly <- paste0("(1", paste(poly_terms, collapse = ""), ")")
  
  cat("Backshift form for y_t (Processed Series):\n")
  cat(formula_poly, "y_t =", round(c_val, 6), "+ e_t\n\n")
  
}

extract_backshift(cases_ar2)
extract_backshift(cases_ar7)
extract_backshift(cases_ar14)


extract_backshift(deaths_ar2)
extract_backshift(deaths_ar7)
extract_backshift(deaths_ar14)



# ==============check if residual is white noise================================
# 1.show residual plots
get_res_diag_plots <- function(model_obj, p_label, type_label) {
  res <- residuals(model_obj)
  
  p_hist <- gghistogram(res, add.normal = TRUE) + 
    # Put type_label in the title here
    labs(title = paste0(type_label, ": AR(", p_label, ") Residual Distribution"), 
         x = "residuals") +
    theme_minimal(base_size = 9)
  
  return(p_hist)
}

# draw plots for cases
row2  <- get_res_diag_plots(cases_ar2,  "2", "Cases")
row7  <- get_res_diag_plots(cases_ar7,  "7", "Cases")
row14 <- get_res_diag_plots(cases_ar14, "14", "Cases")

# draw plots for deaths
row_deaths_2  <- get_res_diag_plots(deaths_ar2,  "2", "Deaths")
row_deaths_7  <- get_res_diag_plots(deaths_ar7,  "7", "Deaths")
row_deaths_14 <- get_res_diag_plots(deaths_ar14, "14", "Deaths")

residual_diag_plot1 <- (row2) / (row7) / (row14)+ plot_annotation(title = "Residual Diagnostics Comparison of Each AR(p): Cases")
residual_diag_plot2 <- (row_deaths_2) / (row_deaths_7) / (row_deaths_14)+ plot_annotation(title = "Residual Diagnostics Comparison of Each AR(p): Deaths")
combined_residual_plot <- residual_diag_plot1 | residual_diag_plot2
print(combined_residual_plot) # None of the residuals are normally distributed

# save plots
ggsave(
  filename = file.path(folder_name, "6. Residual Diagnostics.png"), 
  plot = combined_residual_plot,
  width = 12,     # It is recommended to set width and height to prevent image distortion
  height = 8,
  dpi = 800       # Set resolution to make the plot clearer for the teacher to view
)




# 2. Jarque-Bera Test： H0:Residual is normal distributed
get_jb_results <- function(model_obj, p_label) {
  res <- residuals(model_obj)
  test <- jarque.bera.test(res)
  
  data.frame(
    Model = paste0("AR(", p_label, ")"),
    JB_Statistic = round(test$statistic, 2),
    p_value = format.pval(test$p.value, digits = 4),
    Is_Normal = ifelse(test$p.value > 0.05, "Fail to reject H0", "Reject H0")
  )
}

#JB test for cases
cases_jb_table <- rbind(
  get_jb_results(cases_ar2, "2"),
  get_jb_results(cases_ar7, "7"),
  get_jb_results(cases_ar14, "14")
)

#JB test for deaths
deaths_jb_table <- rbind(
  get_jb_results(deaths_ar2, "2"),
  get_jb_results(deaths_ar7, "7"),
  get_jb_results(deaths_ar14, "14")
)

print(cases_jb_table) # None of the residuals are normally distributed
print(deaths_jb_table) # None of the residuals are normally distributed





#  3. Ljung-Box test: H0: residual is independent(white noise)
get_lb_results <- function(model_obj, p_label) {
  # Dynamically set lag: ensure lag is greater than the number of parameters, taking the larger of p+5 or 10 here
  p_val_count <- length(model_obj$coef) - 1 # Number of AR terms
  test_lag <- max(10, p_val_count + 5) 
  
  lb_test <- Box.test(residuals(model_obj), 
                      lag = test_lag, 
                      type = "Ljung-Box", 
                      fitdf = p_val_count)
  
  data.frame(
    Model = paste0("AR(", p_label, ")"),
    Lag_Used = test_lag, # Record how many lag orders were used
    Statistic = round(lb_test$statistic, 2),
    p_value = lb_test$p.value,
    Is_White_Noise = ifelse(!is.na(lb_test$p.value) && lb_test$p.value > 0.05, "Yes", "No")
  )
}

# LB test for cases
cases_lb_table <- rbind(
  get_lb_results(cases_ar2, "2"),
  get_lb_results(cases_ar7, "7"),
  get_lb_results(cases_ar14, "14")
)

# LB test for deaths
deaths_lb_table <- rbind(
  get_lb_results(deaths_ar2, "2"),
  get_lb_results(deaths_ar7, "7"),
  get_lb_results(deaths_ar14, "14")
)

print(cases_lb_table) # None of the residuals are independent series
print(deaths_lb_table) # None of the residuals are independent series
# Although residuals are not normally distributed, according to OLS theory, this does not affect the consistency of the linear estimator, but will lead to inaccurate confidence intervals






# ======================= Plot fitted vs actual values (fitted on original TS) =====================================

plot_ar_comparison <- function(model_obj, actual_series, p_label) {
  # Prepare data
  df <- data.frame(
    Time = 1:length(actual_series),
    Actual = as.numeric(actual_series),
    Fitted = as.numeric(fitted(model_obj))
  )
  
  # Plotting logic
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), alpha = 0.4, linewidth = 0.5) +
    geom_line(aes(y = Fitted, color = "Fitted"), linewidth = 0.6) +
    scale_color_manual(values = c("Actual" = "grey50", "Fitted" = "firebrick")) + 
    theme_minimal() +
    labs(title = paste0("AR(", p_label, ") Model"), y = "Value", x = "Days") +
    theme(legend.position = "none", title = element_text(size = 10))
}

# Plots for the four cases AR models
cases_p2  <- plot_ar_comparison(cases_ar2,  cases_processed, "2")
cases_p7  <- plot_ar_comparison(cases_ar7,  cases_processed, "7")
cases_p14 <- plot_ar_comparison(cases_ar14, cases_processed, "14")
Cases_AR_comparison <- cases_p2 / cases_p7 / cases_p14  + plot_annotation(title = "Cases AR Models: Fitted vs Actual")

# Plots for the four deaths AR models
deaths_p2  <- plot_ar_comparison(deaths_ar2,  deaths_processed, "2")
deaths_p7  <- plot_ar_comparison(deaths_ar7,  deaths_processed, "7")
deaths_p14 <- plot_ar_comparison(deaths_ar14, deaths_processed, "14")
Deaths_AR_comparison <- deaths_p2 / deaths_p7 / deaths_p14  + plot_annotation(title = "Deaths AR Models: Fitted vs Actual")


AR_comparison <- Cases_AR_comparison | Deaths_AR_comparison
print(Cases_AR_comparison | Deaths_AR_comparison)
# save plots
ggsave(
  filename = file.path(folder_name, "7. AR_Fit_comparison.png"), 
  plot = AR_comparison,
  width = 10,     # It is recommended to set width and height to prevent image distortion
  height = 8,
  dpi = 1000       # Set resolution to make the plot clearer for the teacher to view
)



#===============================================================================
#----------------------------6.1 In-sample forecasting (original time series data, unpartitioned sample)
#===============================================================================

# Assuming your original data is stored in daily_cases$new_cases
# And log_cases <- log1p(daily_cases$new_cases)

in_sample_forecasting <- function(model_obj, raw_series, p_label) {
  raw_series <- as.numeric(raw_series)
  # 1. Regenerate the complete Log series
  y_log <- log1p(raw_series)
  n <- length(y_log)
  
  # 2. Get model fitted values (y_hat)
  # Its length should be n - 8
  y_hat <- as.numeric(fitted(model_obj))
  
  # 3. Strictly align indices for restoration
  # We start forecasting from the 9th point up to the nth point
  # y_hat[1] corresponds to the 9th day of the original series
  
  idx_target <- 9:n
  
  # Execute inverse transformation logic
  lag1 <- as.numeric(y_log[idx_target - 1])
  lag7 <- as.numeric(y_log[idx_target - 7])
  lag8 <- as.numeric(y_log[idx_target - 8])
  
  pred_log <- y_hat + lag1 + lag7 - lag8
  
  # 4. Reverse Log transformation to restore back to case numbers
  pred_cases <- expm1(pred_log)
  actual_cases <- raw_series[idx_target]
  
  # 5. Calculate MSFE
  msfe <- mean((actual_cases - pred_cases)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # 6. Plotting
  df_plot <- data.frame(
    Day = idx_target,
    Actual = actual_cases,
    Predicted = pred_cases
  )
  
  # save plot
  p <- ggplot(df_plot, aes(x = Day)) +
    geom_line(aes(y = Actual, color = "Actual"), alpha = 0.5, linewidth = 0.6) +
    geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", linewidth = 0.6) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "firebrick")) +
    labs(title = paste0("AR(", p_label, ") | RMSE: ", round(rmse, 2)),
         subtitle = paste0("MSFE: ", format(msfe, scientific = FALSE, digits = 4)),
         y = "Number", x = "Days") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(size = 10))
  return(list(msfe = msfe, rmse = rmse, plot = p))
}


# Call function (ensure original log series and original case series are passed in)
cases_ar2_insample  <- in_sample_forecasting(cases_ar2, daily_cases$new_cases, "2")
cases_ar7_insample  <- in_sample_forecasting(cases_ar7,   daily_cases$new_cases, "7")
cases_ar14_insample  <- in_sample_forecasting(cases_ar14, daily_cases$new_cases, "14")

deaths_ar2_insample  <- in_sample_forecasting(deaths_ar2, daily_deaths$deaths, "2")
deaths_ar7_insample  <- in_sample_forecasting(deaths_ar7,   daily_deaths$deaths, "7")
deaths_ar14_insample  <- in_sample_forecasting(deaths_ar14, daily_deaths$deaths, "14")

# Print MSFE comparison table for cases
cases_msfe_summary <- data.frame(
  Model = c("Cases_AR(2)", "Cases_AR(7)", "Cases_AR(14)"),
  MSFE = c(cases_ar2_insample$msfe, cases_ar7_insample$msfe, cases_ar14_insample$msfe)
)
print(cases_msfe_summary)

# Print MSFE comparison table for deaths
deaths_msfe_summary <- data.frame(
  Model = c("Deaths_AR(2)", "Deaths_AR(7)", "Deaths_AR(14)"),
  MSFE = c(deaths_ar2_insample$msfe, deaths_ar7_insample$msfe, deaths_ar14_insample$msfe)
)
print(deaths_msfe_summary)


# In-sample forecasting plots for cases and deaths
cases_insample_plot <- (cases_ar2_insample$plot / cases_ar7_insample$plot / cases_ar14_insample$plot) +plot_annotation(title = "In-sample forecasting for Cases",theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
deaths_insample_plot <- (deaths_ar2_insample$plot / deaths_ar7_insample$plot / deaths_ar14_insample$plot) +plot_annotation(title = "In-sample forecasting for Deaths",theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))

# 3. Use wrap_elements to combine plots
in_sample_forecasting_plot <- wrap_elements(cases_insample_plot) | wrap_elements(deaths_insample_plot)
print(in_sample_forecasting_plot)

# save the plot
ggsave(
  filename = file.path(folder_name, "8. In_Sample_Forecasting.png"), 
  plot = in_sample_forecasting_plot,
  width = 18,        # Increase width to accommodate left and right columns
  height = 10,       # Significantly increase height to accommodate the three vertical models
  dpi = 1000         # Tip: 300-600 DPI is already very clear and the file size is moderate, 1000 may lead to an extremely large file
)

# The prediction results are good. As the autoregressive order p increases, the RMSE and MSFE of the model show a decreasing trend, meaning the prediction effect is getting better.
# This is consistent with theory, because in an in-sample evaluation, increasing model parameters (higher p order) will inevitably absorb more residual fluctuations, leading to better in-sample predictions.
# But this perfect prediction is illusory.

# But why can't it be used in practice? Because when we use the full sample to fit the model and then use this model for prediction,
# it is equivalent to using future observations to predict a past time point t. This is known as Look-ahead Bias in time series analysis.
# In a real forecasting scenario, we can only use data up to time t to predict future values, and cannot use future data to calibrate parameters for past predictions.



#===============================================================================
#----------------------------6.2 In-sample forecasting (log-transformed time series data, unpartitioned sample, one-step ahead forecasting)
#===============================================================================

preprocess_log_ts <- function(series) {
  # 1. Logarithm
  y_final <- log1p(series)
  return(y_final)
}

cases_log_processed <- preprocess_log_ts(daily_cases$new_cases)
deaths_log_processed <- preprocess_log_ts(daily_deaths$deaths)
# Plot the time series after log transformation
plot_data <- data.frame(date = daily_cases$Meldedatum,log_cases = cases_log_processed)
ggplot(plot_data, aes(x = date, y = log_cases)) +geom_line(color = "steelblue") +theme_minimal() +labs(title = "Daily Cases")






in_sample_log_forecasting <- function(model_obj, raw_series, p_label) {
  raw_series <- as.numeric(raw_series)
  # 1. Regenerate the complete Log series (this is our target level)
  y_log <- log1p(raw_series)
  n <- length(y_log)
  
  # 2. Get model fitted values (these are the residuals/predicted values after differencing)
  y_hat <- as.numeric(fitted(model_obj))
  
  # 3. Strictly align indices for restoration (restore differencing, but not Log)
  # Assume the model lost the first 8 points
  idx_target <- 9:n
  
  lag1 <- as.numeric(y_log[idx_target - 1])
  lag7 <- as.numeric(y_log[idx_target - 7])
  lag8 <- as.numeric(y_log[idx_target - 8])
  
  # Restore double differencing to get the predicted values at the log level
  pred_log <- y_hat + lag1 + lag7 - lag8
  
  # 4. Determine the actual values for comparison (also at the log level)
  actual_log <- y_log[idx_target]
  
  # 5. Calculate MSFE (based on Log data)
  msfe <- mean((actual_log - pred_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # 6. Plotting
  df_plot <- data.frame(
    Day = idx_target,
    Actual = actual_log,
    Predicted = pred_log
  )
  
  p <- ggplot(df_plot, aes(x = Day)) +
    geom_line(aes(y = Actual, color = "Actual Log"), alpha = 0.5, linewidth = 0.6) +
    geom_line(aes(y = Predicted, color = "Predicted Log"), linetype = "dashed", linewidth = 0.6) +
    scale_color_manual(values = c("Actual Log" = "black", "Predicted Log" = "firebrick")) +
    labs(title = paste0("AR(", p_label, ") on Log Scale | RMSE: ", round(rmse, 4)),
         subtitle = paste0("MSFE: ", format(msfe, scientific = FALSE, digits = 4)),
         y = "Log(Cases + 1)", x = "Days") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(size = 10))
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}

# --- 1. Cases model forecasting ---
cases_ar2_log_res  <- in_sample_log_forecasting(cases_ar2,  daily_cases$new_cases, "2")
cases_ar7_log_res  <- in_sample_log_forecasting(cases_ar7,  daily_cases$new_cases, "7")
cases_ar14_log_res <- in_sample_log_forecasting(cases_ar14, daily_cases$new_cases, "14")

# --- 2. Deaths model forecasting ---
deaths_ar2_log_res  <- in_sample_log_forecasting(deaths_ar2,  daily_deaths$deaths, "2")
deaths_ar7_log_res  <- in_sample_log_forecasting(deaths_ar7,  daily_deaths$deaths, "7")
deaths_ar14_log_res <- in_sample_log_forecasting(deaths_ar14, daily_deaths$deaths, "14")

# --- 3. Summarize MSFE table ---
msfe_summary <- data.frame(
  Model_Order = c("AR(2)", "AR(7)", "AR(14)"),
  Cases_Log_MSFE = c(cases_ar2_log_res$msfe, cases_ar7_log_res$msfe, cases_ar14_log_res$msfe),
  Deaths_Log_MSFE = c(deaths_ar2_log_res$msfe, deaths_ar7_log_res$msfe, deaths_ar14_log_res$msfe)
)

print("In-sample MSFE Comparison (Log Scale):")
print(msfe_summary)



# Combine the three plots for Cases
cases_plots <- (cases_ar2_log_res$plot / cases_ar7_log_res$plot / cases_ar14_log_res$plot) +
  plot_annotation(title = "Cases: In-sample Forecasting (Log Level)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

# Combine the three plots for Deaths
deaths_plots <- (deaths_ar2_log_res$plot / deaths_ar7_log_res$plot / deaths_ar14_log_res$plot) +
  plot_annotation(title = "Deaths: In-sample Forecasting (Log Level)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

# Finally, display side-by-side left and right
final_comparison_plot <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(final_comparison_plot)

ggsave(
  filename = file.path(folder_name, "9. Log_In_Sample_Forecasting.png"), 
  plot = final_comparison_plot,
  width = 18,        # Increase width to accommodate left and right columns
  height = 10,       # Significantly increase height to accommodate the three vertical models
  dpi = 1000         # Tip: 300-600 DPI is already very clear and the file size is moderate, 1000 may lead to an extremely large file
)



#===============================================================================
#--------------------------6.3 In-sample forecasting (log-transformed time series data, unpartitioned sample, multi-step forecasting)
#===============================================================================
# --- Modified training code ---
# --- 1. Data preprocessing (ensure it is at the Log level) ---
# Assuming daily_cases$new_cases are raw values
y_log_cases  <- log1p(daily_cases$new_cases)
y_log_deaths <- log1p(daily_deaths$deaths)

# --- 2. Train model (Exam focus: specify d=1 and seasonal in Arima) ---
# Here order=c(p, 1, 0) corresponds to (1-L)
# seasonal=list(order=c(0, 1, 0), period=7) corresponds to (1-L^7)
# This way the model includes the lag1, lag7, lag8 logic you calculated manually before
cases_ar2_model  <- Arima(y_log_cases, order = c(2, 1, 0), 
                          seasonal = list(order = c(0, 1, 0), period = 7))
cases_ar7_model  <- Arima(y_log_cases, order = c(7, 1, 0), 
                          seasonal = list(order = c(0, 1, 0), period = 7))
cases_ar14_model <- Arima(y_log_cases, order = c(14, 1, 0), 
                          seasonal = list(order = c(0, 1, 0), period = 7))

# --- 3. Multi-step forecasting plotting function ---
multi_step_plot <- function(model_obj, p_label, h = 28) {
  # Perform multi-step forecasting
  fc <- forecast(model_obj, h = h)
  
  # Get the last 100 days of historical data for display, ensuring the scale is consistent with your Daily Cases plot
  n <- length(model_obj$x)
  history_days <- 100 
  
  df_hist <- data.frame(
    Day = (n - history_days + 1):n,
    Value = as.numeric(model_obj$x[(n - history_days + 1):n]) # This is the Log Level here
  )
  
  df_fc <- data.frame(
    Day = (n + 1):(n + h),
    Point = as.numeric(fc$mean),
    Lo95 = as.numeric(fc$lower[,2]),
    Hi95 = as.numeric(fc$upper[,2])
  )
  
  # Plotting
  ggplot() +
    geom_line(data = df_hist, aes(x = Day, y = Value), color = "steelblue", linewidth = 0.8) +
    geom_ribbon(data = df_fc, aes(x = Day, ymin = Lo95, ymax = Hi95), fill = "firebrick", alpha = 0.2) +
    geom_line(data = df_fc, aes(x = Day, y = Point), color = "firebrick", linetype = "dashed", linewidth = 0.8) +
    labs(title = paste0("AR(", p_label, ") Multi-step Forecast on Log Scale"),
         subtitle = "Shaded: 95% Prediction Interval",
         y = "log_cases", x = "Days") +
    theme_minimal()
}

# --- 4. Generate comparison plots ---
p1 <- multi_step_plot(cases_ar2_model, "2")
p2 <- multi_step_plot(cases_ar7_model, "7")
p3 <- multi_step_plot(cases_ar14_model, "14")

# Use patchwork to combine
library(patchwork)
(p1 / p2 / p3)



#===============================================================================
#----------------------------7.1 In-sample forecasting (log time series data, 70% train, 30% test)
#===============================================================================
# Set split ratio
# 1. Prepare full data on Log scale (used to restore lag terms)
y_log_cases_full <- log1p(daily_cases$new_cases)
y_log_deaths_full <- log1p(daily_deaths$deaths)

# Set split ratio
train_ratio <- 0.7

# 2. Split the stationarized series (used for model training)
n_cases <- length(cases_processed)
split_cases <- floor(n_cases * train_ratio)
train_cases <- cases_processed[1:split_cases]
test_cases  <- cases_processed[(split_cases + 1):n_cases]

n_deaths <- length(deaths_processed)
split_deaths <- floor(n_deaths * train_ratio)
train_deaths <- deaths_processed[1:split_deaths]
test_deaths  <- deaths_processed[(split_deaths + 1):n_deaths]

# --- Train AR models ---
cases_ar2_train  <- Arima(train_cases, order = c(2, 0, 0), include.mean = TRUE)
cases_ar7_train  <- Arima(train_cases, order = c(7, 0, 0), include.mean = TRUE)
cases_ar14_train <- Arima(train_cases, order = c(14, 0, 0), include.mean = TRUE)

deaths_ar2_train  <- Arima(train_deaths, order = c(2, 0, 0), include.mean = TRUE)
deaths_ar7_train  <- Arima(train_deaths, order = c(7, 0, 0), include.mean = TRUE)
deaths_ar14_train <- Arima(train_deaths, order = c(14, 0, 0), include.mean = TRUE)

# --- 3. Define: Out-of-sample dynamic forecasting function on Log scale ---
evaluate_oos_log_fixed <- function(model_obj, full_log_series, full_processed_series, split_idx) {
  n_total_log <- length(full_log_series)
  n_total_proc <- length(full_processed_series)
  
  # 1. Key: Get the "one-step fitted values" of the stationary series on the full sample (Static Fitted Values)
  # Use the trained model parameters and apply them to the entire series (including the test set portion)
  # The resulting z_hat for each day is predicted based on actual past values
  full_fitted_z <- fitted(Arima(full_processed_series, model = model_obj))
  
  # 2. Determine the range of the test set
  # Assume processed has 8 fewer points than log (differencing loss)
  diff_len <- n_total_log - n_total_proc
  h_len <- n_total_proc - split_idx
  
  pred_log <- numeric(h_len)
  actual_log <- numeric(h_len)
  
  # 3. Restoration logic: One-step Ahead (using actual lag terms)
  for (i in 1:h_len) {
    # Current index in the proc series
    idx_p <- split_idx + i
    # Corresponding log series index (compensating for differencing loss)
    idx_l <- idx_p + diff_len
    
    # z_hat is the one-step forecast value
    z_hat_t <- full_fitted_z[idx_p]
    
    # [Core Modification]: Use the actual past values from full_log_series for restoration, instead of predicted values
    pred_log[i] <- z_hat_t + full_log_series[idx_l-1] + full_log_series[idx_l-7] - full_log_series[idx_l-8]
    actual_log[i] <- full_log_series[idx_l]
  }
  
  # Calculate error on the Log scale (now this RMSE is meaningful)
  msfe <- mean((actual_log - pred_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  return(list(pred = pred_log, msfe = msfe, rmse = rmse))
}

# --- Call again (Note: now the full processed series needs to be passed in for fitted calculation) ---
# Take Cases AR(2) as an example
c2_res  <- evaluate_oos_log_fixed(cases_ar2_train,  y_log_cases_full,  cases_processed,  split_cases)
c7_res  <- evaluate_oos_log_fixed(cases_ar7_train,  y_log_cases_full,  cases_processed,  split_cases)
c14_res <- evaluate_oos_log_fixed(cases_ar14_train, y_log_cases_full,  cases_processed,  split_cases)

# Same for Deaths
d2_res  <- evaluate_oos_log_fixed(deaths_ar2_train,  y_log_deaths_full, deaths_processed, split_deaths)
d7_res  <- evaluate_oos_log_fixed(deaths_ar7_train,  y_log_deaths_full, deaths_processed, split_deaths)
d14_res <- evaluate_oos_log_fixed(deaths_ar14_train, y_log_deaths_full, deaths_processed, split_deaths)



# --- 1. Print error comparison table (based on corrected One-step RMSE) ---
# The RMSE at this point reflects the model's ability to capture daily fluctuations, typically ranging between 0.1 - 0.6
oos_summary <- data.frame(
  Model = c("AR(2)", "AR(7)", "AR(14)"),
  Cases_Log_RMSE = c(c2_res$rmse, c7_res$rmse, c14_res$rmse),
  Cases_Log_MSFE = c(c2_res$msfe, c7_res$msfe, c14_res$msfe), # Added MSFE
  Deaths_Log_RMSE = c(d2_res$rmse, d7_res$rmse, d14_res$rmse),
  Deaths_Log_MSFE = c(d2_res$msfe, d7_res$msfe, d14_res$msfe)  # Added MSFE
)
print("Out-of-sample One-step Comparison (Log Scale):")
print(oos_summary)

# --- 2. Correction: Log scale visualization function (add MSFE display) ---
plot_full_forecast_log_fixed <- function(full_log_data, pred_log_data, split_idx, title_label) {
  n_total <- length(full_log_data)
  n_pred <- length(pred_log_data)
  
  # Calculate offset to align the timeline
  offset <- n_total - (split_idx + n_pred)
  
  df <- data.frame(
    Time = 1:n_total,
    Actual = full_log_data,
    Forecast = c(rep(NA, split_idx + offset), pred_log_data)
  )
  
  # Pre-calculate error metrics for easy call in the subtitle
  actual_test <- tail(df$Actual, n_pred)
  current_msfe <- mean((actual_test - pred_log_data)^2, na.rm = TRUE)
  current_rmse <- sqrt(current_msfe)
  
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual Log"), linewidth = 0.4, alpha = 0.6) +
    geom_line(aes(y = Forecast, color = "One-step Forecast"), linetype = "dashed", linewidth = 0.6) +
    scale_color_manual(values = c("Actual Log" = "grey40", "One-step Forecast" = "firebrick")) +
    labs(
      title = title_label, 
      # Display both RMSE and MSFE in scientific notation format in the subtitle
      subtitle = paste0(
        "RMSE: ", round(current_rmse, 4), 
        " | MSFE: ", format(current_msfe, scientific = FALSE, digits = 4)
      ),
      x = "Days", y = "Log(Value + 1)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(size = 11, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "firebrick")
    )
}

# --- 3. Generate 6 subplots ---
p_c2  <- plot_full_forecast_log_fixed(y_log_cases_full,  c2_res$pred,  split_cases,  "Cases: AR(2)")
p_c7  <- plot_full_forecast_log_fixed(y_log_cases_full,  c7_res$pred,  split_cases,  "Cases: AR(7)")
p_c14 <- plot_full_forecast_log_fixed(y_log_cases_full,  c14_res$pred, split_cases,  "Cases: AR(14)")

p_d2  <- plot_full_forecast_log_fixed(y_log_deaths_full, d2_res$pred,  split_deaths, "Deaths: AR(2)")
p_d7  <- plot_full_forecast_log_fixed(y_log_deaths_full, d7_res$pred,  split_deaths, "Deaths: AR(7)")
p_d14 <- plot_full_forecast_log_fixed(y_log_deaths_full, d14_res$pred, split_deaths, "Deaths: AR(14)")

# --- 4. Combine into a large output plot ---
out_sample_log_comparison <- (
  (p_c2 / p_c7 / p_c14) | (p_d2 / p_d7 / p_d14)
) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(
    title = "AR Model Out-of-sample One-step Forecast (Log Scale)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom"
    )
  )

print(out_sample_log_comparison)


ggsave(
  filename = file.path(folder_name, "10. out_sample_log_comparison.png"), 
  plot = out_sample_log_comparison,
  width = 18,        # Increase width to accommodate left and right columns
  height = 10,       # Significantly increase height to accommodate the three vertical models
  dpi = 1000         # Tip: 300-600 DPI is already very clear and the file size is moderate, 1000 may lead to an extremely large file
)



#===================================================================================
#----------------------------7.2 In-sample forecasting (log time series data, 70% train, 30% test, multi-step forecasting)
#===================================================================================
# Core modification: Set include.mean = FALSE to remove the intercept term
# --- 1. Train pure AR models (no intercept version) ---
# Ensure the differenced and stationarized train_cases and train_deaths are used
preprocess_log_ts <- function(series) {
  # 1. Logarithm
  y_final <- log1p(series)
  return(y_final)
}
cases_log_processed <- preprocess_log_ts(daily_cases$new_cases)
deaths_log_processed <- preprocess_log_ts(daily_deaths$deaths)



cases_ar2_train_no_intercept  <- Arima(train_cases, order = c(2, 0, 0), include.mean = FALSE)
cases_ar7_train_no_intercept  <- Arima(train_cases, order = c(7, 0, 0), include.mean = FALSE)
cases_ar14_train_no_intercept <- Arima(train_cases, order = c(14, 0, 0), include.mean = FALSE)

deaths_ar2_train_no_intercept  <- Arima(train_deaths, order = c(2, 0, 0), include.mean = FALSE)
deaths_ar7_train_no_intercept  <- Arima(train_deaths, order = c(7, 0, 0), include.mean = FALSE)
deaths_ar14_train_no_intercept <- Arima(train_deaths, order = c(14, 0, 0), include.mean = FALSE)

# --- 2. Core: Out-of-sample multi-step recursive forecasting function on Log scale ---
evaluate_multi_step_fixed <- function(model_obj, full_log_series, processed_series, split_idx, h) {
  # 1. Forecast the future h steps of stationary series z (mean will tend to 0 since there is no intercept)
  fc_z <- as.numeric(forecast(model_obj, h = h)$mean)
  
  # 2. Calculate length difference (differencing loss, usually 8)
  diff_gap <- length(full_log_series) - length(processed_series) 
  
  # 3. Determine the end point of the training set on the Log scale
  train_end_log_idx <- split_idx + diff_gap
  
  # Get the actual historical Log values up to the end of training
  history <- as.numeric(full_log_series[1:train_end_log_idx])
  preds_log <- numeric(h)
  
  # 4. Recursive restoration logic: y_t = z_t + y_{t-1} + y_{t-7} - y_{t-8}
  for (i in 1:h) {
    z_t <- fc_z[i]
    n <- length(history)
    
    # Continuously update history with predicted values
    y_next <- z_t + history[n] + history[n-6] - history[n-7]
    
    preds_log[i] <- y_next
    history <- c(history, y_next) 
  }
  
  # 5. Extract corresponding actual values for error calculation
  actual_log <- as.numeric(full_log_series[(train_end_log_idx + 1):(train_end_log_idx + h)])
  
  valid_mask <- !is.na(actual_log)
  rmse <- sqrt(mean((actual_log[valid_mask] - preds_log[valid_mask])^2))
  
  return(list(pred = preds_log, actual = actual_log, rmse = rmse))
}

# --- 3. Run multi-step forecasting ---
h_c <- length(test_cases)
h_d <- length(test_deaths)

# Cases forecasting
c2_ms  <- evaluate_multi_step_fixed(cases_ar2_train_no_intercept,  y_log_cases_full,  cases_processed,  split_cases,  h_c)
c7_ms  <- evaluate_multi_step_fixed(cases_ar7_train_no_intercept,  y_log_cases_full,  cases_processed,  split_cases,  h_c)
c14_ms <- evaluate_multi_step_fixed(cases_ar14_train_no_intercept, y_log_cases_full,  cases_processed,  split_cases,  h_c)

# Deaths forecasting
d2_ms  <- evaluate_multi_step_fixed(deaths_ar2_train_no_intercept,  y_log_deaths_full, deaths_processed, split_deaths, h_d)
d7_ms  <- evaluate_multi_step_fixed(deaths_ar7_train_no_intercept,  y_log_deaths_full, deaths_processed, split_deaths, h_d)
d14_ms <- evaluate_multi_step_fixed(deaths_ar14_train_no_intercept, y_log_deaths_full, deaths_processed, split_deaths, h_d)

# --- 4. Check results comparison table ---
ms_summary <- data.frame(
  Model = c("AR(2)", "AR(7)", "AR(14)"),
  Cases_MS_RMSE = c(c2_ms$rmse, c7_ms$rmse, c14_ms$rmse),
  Deaths_MS_RMSE = c(d2_ms$rmse, d7_ms$rmse, d14_ms$rmse)
)
print("Multi-step Out-of-sample RMSE (No Intercept, Log Scale):")
print(ms_summary)

# --- 5. Visualization function ---
plot_ms_results <- function(full_log, ms_res, title) {
  n_total <- length(full_log)
  h <- length(ms_res$pred)
  
  df <- data.frame(
    Time = 1:n_total,
    Actual = full_log,
    Forecast = c(rep(NA, n_total - h), ms_res$pred)
  )
  
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), alpha = 0.4) +
    geom_line(aes(y = Forecast, color = "Multi-step Forecast"), linewidth = 0.7) +
    scale_color_manual(values = c("Actual" = "grey50", "Multi-step Forecast" = "firebrick")) +
    labs(title = title, 
         subtitle = paste("RMSE:", round(ms_res$rmse, 4)), 
         y = "Log Value", x = "Days") +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(size = 10))
}

# --- 6. Generate comparison plots ---
p1 <- plot_ms_results(y_log_cases_full, c2_ms,  "Cases AR(2) No Intercept")
p2 <- plot_ms_results(y_log_cases_full, c7_ms,  "Cases AR(7) No Intercept")
p3 <- plot_ms_results(y_log_cases_full, c14_ms, "Cases AR(14) No Intercept")

p4 <- plot_ms_results(y_log_deaths_full, d2_ms,  "Deaths AR(2) No Intercept")
p5 <- plot_ms_results(y_log_deaths_full, d7_ms,  "Deaths AR(7) No Intercept")
p6 <- plot_ms_results(y_log_deaths_full, d14_ms, "Deaths AR(14) No Intercept")

(p1 / p2 / p3) | (p4 / p5 / p6) + 
  plot_annotation(title = "Multi-step Forecast: Pure AR Models without Intercept")
#looks wierd...




#===============================================================================
#----------------------------8. add Predictor
#===============================================================================

#  to be continued
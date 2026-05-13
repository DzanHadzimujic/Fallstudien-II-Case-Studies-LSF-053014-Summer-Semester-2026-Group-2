library(dplyr)
library(knitr) #latex
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(forecast)   # for Acf(), Pacf(), and later Arima()
library(zoo)        # for as.Date / time series helpers
library(patchwork)  # for combining ggplots side by side
library(tseries)    # for adf.test() and kpss.test(), Jarque-Bera Test
library(lmtest)
library(jsonlite)
library(future)
library(future.apply)
##==============================================================================
##=============================0. Some Preparation======================================
##==============================================================================
#data_infektionen is downloaded from https://github.com/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland
#immunizations datasets are downloaded from https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/main/Deutschland_Impfquoten_COVID-19.csv

# Identify Working path
working_path <- setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# creat a folder to save plots
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
## Based on Meldedatum, compute infection cases and death cases
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
  group_by(Meldedatum) %>%                                         # Group and sum by each day to obtain daily death data
  summarise(deaths = sum(AnzahlTodesfall), .groups = "drop") %>%   # Add up AnzahlTodesfall from all corresponding rows; .groups = "drop" removes grouping
  arrange(Meldedatum)                                              # Sort the table in chronological order, starting from early 2020




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
  dpi = 500       # Set the resolution so that the plots are clearer for the instructor
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
# Analyze the ACF and PACF plots in the report: the decay is extremely slow, which is typical of ① non-stationarity and ② strong periodicity


# # Use STL decomposition to show the components of the TS
# cases_msts <- msts(daily_cases$new_cases, seasonal.periods = c(7, 365.25))
# cases_msts_decomposition <- mstl(cases_msts)
# plot(cases_msts_decomposition) 
# 
# cases_msts <- msts(daily_deaths$deaths, seasonal.periods = c(7, 365.25))
# cases_msts_decomposition <- mstl(cases_msts)
# plot(cases_msts_decomposition) 




# =============Stationary test: ADF-test and KPSS-test, PP TEST==========================

check_stationarity <- function(series, name) {
  cat("\n==============================================\n")
  cat("  Stationarity Analysis for:", name, "\n")
  cat("==============================================\n")
  
  # 1. ADF test (Augmented Dickey-Fuller)
  # H0: the series is non-stationary
  cat("\n[1] Augmented Dickey-Fuller Test:\n")
  print(adf.test(series))
  
  # 2. PP test (Phillips-Perron)
  # H0: the series is non-stationary; more robust to higher-order serial correlation
  cat("\n[2] Phillips-Perron Unit Root Test:\n")
  print(pp.test(series))
  
  # 3. KPSS test (Kwiatkowski-Phillips-Schmidt-Shin)
  # H0: the series is stationary; the logic is opposite to the first two tests
  cat("\n[3] KPSS Unit Root Test:\n")
  print(kpss.test(series))
}

check_stationarity(daily_cases$new_cases, "Daily Cases")  
check_stationarity(daily_deaths$deaths, "Daily Deaths")

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
  dpi = 600       # Set the resolution so that the plots are clearer for the instructor
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


# Stationary test
check_stationarity(cases_processed, "Processed Daily Cases")   
check_stationarity(deaths_processed, "Processed Daily Deaths") 
# Both TS are stationary, so AR models can be used for forecasting. Next, use AIC, BIC, and HQIC to select the AR order P; then fit the AR model using the full sample; finally, use the Ljung-Box test to check whether the residuals are white noise


#===============================================================================
#---------4. Select AR order
#===============================================================================
# Define a function to automatically loop over and select the order of AR(p)
get_ar_criteria <- function(series, max_p = 15) {
  # Create the results table
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
    k <- length(fit$coef) # number of parameters
    criteria_table$HQIC[p] <- -2 * fit$loglik + 2 * k * log(log(n))
  }
  
  return(criteria_table)
}

# Run the function (will cost a while)
results_cases <- get_ar_criteria(cases_processed)
results_deaths <- get_ar_criteria(deaths_processed)

# Check results
print("Information Criteria for Cases:")
print(results_cases)
print(results_deaths)


# Plot the results
plot_info_criteria <- function(data, title_suffix = "") {
  # 1. Convert to long-format data
  data_long <- data %>%
    pivot_longer(cols = c(AIC, BIC, HQIC), 
                 names_to = "Criterion", 
                 values_to = "Value")
  
  # 2. Generate the plot and return it
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
  height = 12, # Increase the height to accommodate two groups of subplots
  dpi = 800
)


# IC keeps decreasing, so the optimal order cannot be selected
# Compare fixed p = 2, p = 7, and p = 14


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
cat("\n--- Empirical Evidence 2: Information Criteria (IC) Comparison ---\n")
cat("Parsimonious model including MA term (number of parameters = 3) AIC: ", AIC(MA_proof_model), "\n")
cat("Large pure AR model (number of parameters = 14) AIC: ", AIC(AR14_model), "\n")
cat("Parsimonious model including MA term (number of parameters = 3) BIC: ", BIC(MA_proof_model), "\n")
cat("Large pure AR model (number of parameters = 14) BIC: ", BIC(AR14_model), "\n")
# IC decrease significantly after adding MA term
# ➡ We have proved there exists MA structurals




#===============================================================================
#---------5. But as requested in the project ,we need to fit pure AR(p)
#===============================================================================
# Train one AR(2) and one AR(7), respectively
# Fit AR and show results
cases_ar2 <- Arima(cases_processed, order = c(2, 0, 0), include.mean = TRUE)
cases_ar7 <- Arima(cases_processed, order = c(7, 0, 0), include.mean = TRUE)
cases_ar14 <- Arima(cases_processed, order = c(14, 0, 0), include.mean = TRUE)
summary(cases_ar2)
summary(cases_ar7)  # better than AR(2)
summary(cases_ar14) # better than AR(7), but to avoid overfitting, it will not be selected


deaths_ar2 <- Arima(deaths_processed, order = c(2, 0, 0), include.mean = TRUE)
deaths_ar7 <- Arima(deaths_processed, order = c(7, 0, 0), include.mean = TRUE)
deaths_ar14 <- Arima(deaths_processed, order = c(14, 0, 0), include.mean = TRUE)
summary(deaths_ar2)
summary(deaths_ar7)  # better than AR(2)
summary(deaths_ar14) # better than AR(7), but to avoid overfitting, it will not be selected

#==================== backshift representation of A R===========================

extract_backshift <- function(fit_model) {
  # Extract all coefficients
  coefs <- fit_model$coef
  
  # Extract AR coefficients, whose names start with ar
  ar_coefs <- coefs[grep("^ar", names(coefs))]
  
  # Search for the constant term, which may be mean or intercept
  intercept_idx <- which(names(coefs) %in% c("mean", "intercept", "drift"))
  if (length(intercept_idx) > 0) {
    mu <- coefs[intercept_idx]
  } else {
    mu <- 0
  }
  
  # The mean reported by Arima is the mean of the series, mu
  # The constant on the right-hand side of the backshift form is c = mu * (1 - sum(phi_i))
  c_val <- mu * (1 - sum(ar_coefs))
  
  # Construct the polynomial part
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
    # Put type_label into the title here
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
print(combined_residual_plot) # Residuals are not normally distributed

# save plots
ggsave(
  filename = file.path(folder_name, "6. Residual Diagnostics.png"), 
  plot = combined_residual_plot,
  width = 12,     # It is recommended to set width and height to prevent image distortion
  height = 8,
  dpi = 800       # Set the resolution so that the plots are clearer for the instructor
)







# 2. Jarque-Bera Test: H0:Residual is normal distributed
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

print(cases_jb_table) # The residuals are not normally distributed
print(deaths_jb_table) # The residuals are not normally distributed





#  3. Ljung-Box test: H0: residual is independent(white noise)
get_lb_results <- function(model_obj, p_label) {
  # Dynamically set lag: ensure the lag is larger than the number of parameters; here take the larger value between p + 5 and 10
  p_val_count <- length(model_obj$coef) - 1 # number of AR terms
  test_lag <- max(10, p_val_count + 5) 
  
  lb_test <- Box.test(residuals(model_obj), 
                      lag = test_lag, 
                      type = "Ljung-Box", 
                      fitdf = p_val_count)
  
  data.frame(
    Model = paste0("AR(", p_label, ")"),
    Lag_Used = test_lag, # Record how many lags were used
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

print(cases_lb_table) # The residuals are not independent series
print(deaths_lb_table) # The residuals are not independent series
# Although the residuals are not normally distributed, according to OLS theory, this does not affect the consistency of the linear estimators, but it will make the confidence intervals inaccurate






# =======================Plot fitted values vs actual values, fitted on the original TS=====================================

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

# Plots of the four cases AR models
cases_p2  <- plot_ar_comparison(cases_ar2,  cases_processed, "2")
cases_p7  <- plot_ar_comparison(cases_ar7,  cases_processed, "7")
cases_p14 <- plot_ar_comparison(cases_ar14, cases_processed, "14")
Cases_AR_comparison <- cases_p2 / cases_p7 / cases_p14  + plot_annotation(title = "Cases AR Models: Fitted vs Actual")

# Plots of the four deaths AR models
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
  dpi = 1000       # Set the resolution so that the plots are clearer for the instructor
)



#===============================================================================
#----------------------------6.1 In-sample forecasting (original time series data, no sample split)
#===============================================================================

# Assume that your original data are stored in daily_cases$new_cases
# and log_cases <- log1p(daily_cases$new_cases)

in_sample_forecasting <- function(model_obj, raw_series, p_label) {
  raw_series <- as.numeric(raw_series)
  # 1. Regenerate the complete Log series
  y_log <- log1p(raw_series)
  n <- length(y_log)
  
  # 2. Obtain the fitted values of the model (y_hat)
  # Its length should be n - 8
  y_hat <- as.numeric(fitted(model_obj))
  
  # 3. Strictly align indices for reconstruction
  # We start forecasting from the 9th point until the nth point
  # y_hat[1] corresponds to the 9th day of the original series
  
  idx_target <- 9:n
  
  # Execute the inverse transformation logic
  lag1 <- as.numeric(y_log[idx_target - 1])
  lag7 <- as.numeric(y_log[idx_target - 7])
  lag8 <- as.numeric(y_log[idx_target - 8])
  
  pred_log <- y_hat + lag1 + lag7 - lag8
  
  # 4. Reverse the Log transformation back to the number of cases
  pred_cases <- expm1(pred_log)
  actual_cases <- raw_series[idx_target]
  
  # 5. Calculate MSFE
  msfe <- mean((actual_cases - pred_cases)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # 6. Plot
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


# Call the function, making sure to pass the original log series and the original case series
cases_ar2_insample  <- in_sample_forecasting(cases_ar2, daily_cases$new_cases, "2")
cases_ar7_insample  <- in_sample_forecasting(cases_ar7,   daily_cases$new_cases, "7")
cases_ar14_insample  <- in_sample_forecasting(cases_ar14, daily_cases$new_cases, "14")

deaths_ar2_insample  <- in_sample_forecasting(deaths_ar2, daily_deaths$deaths, "2")
deaths_ar7_insample  <- in_sample_forecasting(deaths_ar7,   daily_deaths$deaths, "7")
deaths_ar14_insample  <- in_sample_forecasting(deaths_ar14, daily_deaths$deaths, "14")

# Print the MSFE comparison table for cases
cases_msfe_summary <- data.frame(
  Model = c("Cases_AR(2)", "Cases_AR(7)", "Cases_AR(14)"),
  MSFE = c(cases_ar2_insample$msfe, cases_ar7_insample$msfe, cases_ar14_insample$msfe)
)
print(cases_msfe_summary)

# Print the MSFE comparison table for deaths
deaths_msfe_summary <- data.frame(
  Model = c("Deaths_AR(2)", "Deaths_AR(7)", "Deaths_AR(14)"),
  MSFE = c(deaths_ar2_insample$msfe, deaths_ar7_insample$msfe, deaths_ar14_insample$msfe)
)
print(deaths_msfe_summary)


# Summarize the MSFE table ---
origindata_msfe_summary <- data.frame(
  Model_Order = c("AR(2)", "AR(7)", "AR(14)"),
  Cases_Log_MSFE = c(cases_ar2_insample$msfe, cases_ar7_insample$msfe, cases_ar14_insample$msfe),
  Deaths_Log_MSFE = c(deaths_ar2_insample$msfe, deaths_ar7_insample$msfe, deaths_ar14_insample$msfe)
)
print("In-sample MSFE Comparison (Original data):")
print(origindata_msfe_summary)



# In-sample forecasting plots for cases and deaths
cases_insample_plot <- (cases_ar2_insample$plot / cases_ar7_insample$plot / cases_ar14_insample$plot) +plot_annotation(title = "In-sample forecasting for Cases",theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))
deaths_insample_plot <- (deaths_ar2_insample$plot / deaths_ar7_insample$plot / deaths_ar14_insample$plot) +plot_annotation(title = "In-sample forecasting for Deaths",theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")))

# 3. Use wrap_elements to combine the plots
in_sample_forecasting_plot <- wrap_elements(cases_insample_plot) | wrap_elements(deaths_insample_plot)
print(in_sample_forecasting_plot)

# save the plot
ggsave(
  filename = file.path(folder_name, "8. In_Sample_Forecasting.png"), 
  plot = in_sample_forecasting_plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 1000         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)


#===============================================================================
#----------------------------6.2 In-sample forecasting (log-transformed time series data, no sample split, one-step forecasting)
#===============================================================================

preprocess_log_ts <- function(series) {
  # 1. Logarithm
  y_final <- log1p(series)
  return(y_final)
}

cases_log_processed <- preprocess_log_ts(daily_cases$new_cases)
deaths_log_processed <- preprocess_log_ts(daily_deaths$deaths)
str(cases_log_processed)


# Plot the time series after log transformation
plot_data <- data.frame(date = daily_cases$Meldedatum,log_cases = cases_log_processed)
ggplot(plot_data, aes(x = date, y = log_cases)) +geom_line(color = "steelblue") +theme_minimal() +labs(title = "Daily Cases")




in_sample_log_forecasting <- function(model_obj, raw_series, p_label) {
  raw_series <- as.numeric(raw_series)
  # 1. Regenerate the complete Log series, which is our target level
  y_log <- log1p(raw_series)
  n <- length(y_log)
  
  # 2. Obtain the fitted values of the model, which are the residuals/predicted values after differencing
  y_hat <- as.numeric(fitted(model_obj))
  
  # 3. Strictly align indices for reconstruction, reconstructing differencing but not reversing the Log transformation
  # Assume that the model loses the first 8 points
  idx_target <- 9:n
  
  lag1 <- as.numeric(y_log[idx_target - 1])
  lag7 <- as.numeric(y_log[idx_target - 7])
  lag8 <- as.numeric(y_log[idx_target - 8])
  
  # Reverse the double differencing to obtain predicted values at the log level
  pred_log <- y_hat + lag1 + lag7 - lag8
  
  # 4. Determine the true values for comparison, also at the log level
  actual_log <- y_log[idx_target]
  
  # 5. Calculate MSFE based on Log data
  msfe <- mean((actual_log - pred_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # 6. Plot
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

# --- 1. Forecasting for Cases models ---
cases_ar2_log_res  <- in_sample_log_forecasting(cases_ar2,  daily_cases$new_cases, "2")
cases_ar7_log_res  <- in_sample_log_forecasting(cases_ar7,  daily_cases$new_cases, "7")
cases_ar14_log_res <- in_sample_log_forecasting(cases_ar14, daily_cases$new_cases, "14")

# --- 2. Forecasting for Deaths models ---
deaths_ar2_log_res  <- in_sample_log_forecasting(deaths_ar2,  daily_deaths$deaths, "2")
deaths_ar7_log_res  <- in_sample_log_forecasting(deaths_ar7,  daily_deaths$deaths, "7")
deaths_ar14_log_res <- in_sample_log_forecasting(deaths_ar14, daily_deaths$deaths, "14")

# --- 3. Summarize the MSFE table ---
logdata_msfe_summary <- data.frame(
  Model_Order = c("AR(2)", "AR(7)", "AR(14)"),
  Cases_Log_MSFE = c(cases_ar2_log_res$msfe, cases_ar7_log_res$msfe, cases_ar14_log_res$msfe),
  Deaths_Log_MSFE = c(deaths_ar2_log_res$msfe, deaths_ar7_log_res$msfe, deaths_ar14_log_res$msfe)
)

print("In-sample MSFE Comparison (Log Scale):")
print(logdata_msfe_summary)



# Combine the three plots for Cases
cases_plots <- (cases_ar2_log_res$plot / cases_ar7_log_res$plot / cases_ar14_log_res$plot) +
  plot_annotation(title = "Cases: In-sample Forecasting (Log Level)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

# Combine the three plots for Deaths
deaths_plots <- (deaths_ar2_log_res$plot / deaths_ar7_log_res$plot / deaths_ar14_log_res$plot) +
  plot_annotation(title = "Deaths: In-sample Forecasting (Log Level)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

# Final left-right side-by-side display
final_comparison_plot <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(final_comparison_plot)

ggsave(
  filename = file.path(folder_name, "9. Log_In_Sample_Forecasting.png"), 
  plot = final_comparison_plot,   
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 1000         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)


#===============================================================================
#--------------------------6.3 In-sample forecasting (log-transformed time series data, no sample split, multi-step forecasting)
#===============================================================================
in_sample_multistep_log_forecasting <- function(model_obj, raw_series, p_label, h = 30, y_label = "Log(Value + 1)") {
  raw_series <- as.numeric(raw_series)
  y_log <- log1p(raw_series)
  n <- length(y_log)
  
  split_idx <- n - h
  y_log_trunc <- y_log[1:split_idx]
  
  # Use the full-sample model parameters and refit them to the truncated sample
  historical_model <- Arima(y_log_trunc, model = model_obj)
  
  # Here forecast() is theoretically consistent
  fc <- forecast(historical_model, h = h, level = 95)
  
  preds_log <- as.numeric(fc$mean)
  lower_log <- as.numeric(fc$lower[, 1])
  upper_log <- as.numeric(fc$upper[, 1])
  
  actual_log <- y_log[(split_idx + 1):n]
  msfe <- mean((actual_log - preds_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  plot_start <- max(1, split_idx - 70)
  
  df_actual <- data.frame(
    Day = plot_start:n,
    Value = y_log[plot_start:n]
  )
  
  df_pred <- data.frame(
    Day = (split_idx + 1):n,
    Point = preds_log,
    Lower = lower_log,
    Upper = upper_log
  )
  
  connect_x <- split_idx
  connect_y <- y_log[split_idx]
  
  y_max <- max(c(df_actual$Value, df_pred$Upper), na.rm = TRUE) * 1.05
  y_min <- min(c(df_actual$Value, df_pred$Lower), na.rm = TRUE) * 0.95
  
  p <- ggplot() +
    geom_ribbon(
      data = df_pred,
      aes(x = Day, ymin = Lower, ymax = Upper, fill = "95% Prediction Interval"),
      alpha = 0.2
    ) +
    geom_line(
      data = df_actual,
      aes(x = Day, y = Value, color = "Actual Data"),
      linewidth = 0.6
    ) +
    geom_line(
      data = df_pred,
      aes(x = Day, y = Point, color = "Dynamic Forecast"),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_segment(
      aes(x = connect_x, y = connect_y, xend = split_idx + 1, yend = preds_log[1]),
      color = "firebrick", linetype = "dashed", linewidth = 0.8
    ) +
    geom_vline(xintercept = split_idx, linetype = "dotted", color = "grey50") +
    scale_color_manual(name = "", values = c("Actual Data" = "black", "Dynamic Forecast" = "firebrick")) +
    scale_fill_manual(name = "", values = c("95% Prediction Interval" = "firebrick")) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(
      title = paste0("AR(", p_label, ") In-sample Dynamic (h=", h, ") | RMSE: ", round(rmse, 4)),
      subtitle = paste0("MSFE: ", format(msfe, scientific = FALSE, digits = 4)),
      y = y_label,
      x = "Days"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = -10),
      plot.title = element_text(size = 10, face = "bold")
    )
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}

# ==========================================================
# Run and generate plots
# ==========================================================
y_log_cases_full <- log1p(daily_cases$new_cases)
y_log_deaths_full <- log1p(daily_deaths$deaths)


cases_ar2_dyn  <- Arima(y_log_cases_full, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE, method = "ML")
cases_ar7_dyn  <- Arima(y_log_cases_full, order = c(7, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE, method = "ML")
cases_ar14_dyn <- Arima(y_log_cases_full, order = c(14, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE, method = "ML")

deaths_ar2_dyn  <- Arima(y_log_deaths_full, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE, method = "ML")
deaths_ar7_dyn  <- Arima(y_log_deaths_full, order = c(7, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE, method = "ML")
deaths_ar14_dyn <- Arima(y_log_deaths_full, order = c(14, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE, method = "ML")


forecast_horizon <- 30 
# Note: the input models such as cases_ar2 must be AR models trained on the full-sample cases_processed series
cases_ar2_log_res  <- in_sample_multistep_log_forecasting(cases_ar2_dyn,  daily_cases$new_cases, "2",  h = forecast_horizon, y_label = "Log(Cases + 1)")
cases_ar7_log_res  <- in_sample_multistep_log_forecasting(cases_ar7_dyn,  daily_cases$new_cases, "7",  h = forecast_horizon, y_label = "Log(Cases + 1)")
cases_ar14_log_res <- in_sample_multistep_log_forecasting(cases_ar14_dyn, daily_cases$new_cases, "14", h = forecast_horizon, y_label = "Log(Cases + 1)")

deaths_ar2_log_res  <- in_sample_multistep_log_forecasting(deaths_ar2_dyn,  daily_deaths$deaths, "2",  h = forecast_horizon, y_label = "Log(Deaths + 1)")
deaths_ar7_log_res  <- in_sample_multistep_log_forecasting(deaths_ar7_dyn,  daily_deaths$deaths, "7",  h = forecast_horizon, y_label = "Log(Deaths + 1)")
deaths_ar14_log_res <- in_sample_multistep_log_forecasting(deaths_ar14_dyn, daily_deaths$deaths, "14", h = forecast_horizon, y_label = "Log(Deaths + 1)")
# Combine into a large plot
cases_plots <- (cases_ar2_log_res$plot / cases_ar7_log_res$plot / cases_ar14_log_res$plot) +
  plot_annotation(title = "Cases: In-sample Dynamic Forecast (Log Level)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

deaths_plots <- (deaths_ar2_log_res$plot / deaths_ar7_log_res$plot / deaths_ar14_log_res$plot) +
  plot_annotation(title = "Deaths: In-sample Dynamic Forecast (Log Level)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

multistep_insample_forecast <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(multistep_insample_forecast)

ggsave(
  filename = file.path(folder_name, "10. multistep_insample_forecast.png"), 
  plot = multistep_insample_forecast,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)





#===============================================================================
#------------------7.1 Out-of-sample forecasting (log time series data, 70% training, 30% testing, one-step forecasting)
#===============================================================================
# Set the split ratio
# 1. Prepare the full data on the Log scale, used to reconstruct lagged terms
y_log_cases_full <- log1p(daily_cases$new_cases)
y_log_deaths_full <- log1p(daily_deaths$deaths)

# Set the split ratio
train_ratio <- 0.7

# 2. Split the stationarized series, used for model training
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

# --- 3. Define: out-of-sample dynamic forecasting function on the Log scale ---
evaluate_oos_log_fixed <- function(model_obj, full_log_series, full_processed_series, split_idx) {
  n_total_log <- length(full_log_series)
  n_total_proc <- length(full_processed_series)
  
  # 1. Key step: obtain one-step fitted values on the full sample of the stationary series (Static Fitted Values)
  # Use the trained model parameters and apply them to the entire series, including the test-set part
  # In this way, the z_hat obtained for each day is based on the true past values
  full_fitted_z <- fitted(Arima(full_processed_series, model = model_obj))
  
  # 2. Determine the range of the test set
  # Assume that processed has 8 fewer points than log because of differencing loss
  diff_len <- n_total_log - n_total_proc
  h_len <- n_total_proc - split_idx
  
  pred_log <- numeric(h_len)
  actual_log <- numeric(h_len)
  
  # 3. Reconstruction logic: One-step Ahead, using true lagged terms
  for (i in 1:h_len) {
    # Current index in the proc series
    idx_p <- split_idx + i
    # Corresponding index in the log series, compensating for the differencing loss
    idx_l <- idx_p + diff_len
    
    # z_hat is the one-step predicted value
    z_hat_t <- full_fitted_z[idx_p]
    
    # Core modification: use the true past values from full_log_series for reconstruction, instead of using predicted values
    pred_log[i] <- z_hat_t + full_log_series[idx_l-1] + full_log_series[idx_l-7] - full_log_series[idx_l-8]
    actual_log[i] <- full_log_series[idx_l]
  }
  
  # Calculate the error on the Log scale; now this RMSE is meaningful
  msfe <- mean((actual_log - pred_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  return(list(pred = pred_log, msfe = msfe, rmse = rmse))
}

# --- Call again, noting that the full processed series is now required for fitted-value calculation ---
# Take Cases AR(2) as an example
c2_res  <- evaluate_oos_log_fixed(cases_ar2_train,  y_log_cases_full,  cases_processed,  split_cases)
c7_res  <- evaluate_oos_log_fixed(cases_ar7_train,  y_log_cases_full,  cases_processed,  split_cases)
c14_res <- evaluate_oos_log_fixed(cases_ar14_train, y_log_cases_full,  cases_processed,  split_cases)

# Same for Deaths
d2_res  <- evaluate_oos_log_fixed(deaths_ar2_train,  y_log_deaths_full, deaths_processed, split_deaths)
d7_res  <- evaluate_oos_log_fixed(deaths_ar7_train,  y_log_deaths_full, deaths_processed, split_deaths)
d14_res <- evaluate_oos_log_fixed(deaths_ar14_train, y_log_deaths_full, deaths_processed, split_deaths)



# --- 1. Print the error comparison table, based on the corrected One-step RMSE ---
# The RMSE here reflects the model's ability to capture daily fluctuations; its value is usually between 0.1 and 0.6
oos_summary <- data.frame(
  Model = c("AR(2)", "AR(7)", "AR(14)"),
  Cases_Log_RMSE = c(c2_res$rmse, c7_res$rmse, c14_res$rmse),
  Cases_Log_MSFE = c(c2_res$msfe, c7_res$msfe, c14_res$msfe), # Add MSFE
  Deaths_Log_RMSE = c(d2_res$rmse, d7_res$rmse, d14_res$rmse),
  Deaths_Log_MSFE = c(d2_res$msfe, d7_res$msfe, d14_res$msfe)  # Add MSFE
)
print("Out-of-sample One-step Comparison (Log Scale):")
print(oos_summary)

# --- 2. Correction: Log-scale visualization function, adding MSFE display ---
plot_full_forecast_log_fixed <- function(full_log_data, pred_log_data, split_idx, title_label) {
  n_total <- length(full_log_data)
  n_pred <- length(pred_log_data)
  
  # Calculate the offset to align the time axis
  offset <- n_total - (split_idx + n_pred)
  
  df <- data.frame(
    Time = 1:n_total,
    Actual = full_log_data,
    Forecast = c(rep(NA, split_idx + offset), pred_log_data)
  )
  
  # Pre-calculate error metrics for use in the subtitle
  actual_test <- tail(df$Actual, n_pred)
  current_msfe <- mean((actual_test - pred_log_data)^2, na.rm = TRUE)
  current_rmse <- sqrt(current_msfe)
  
  ggplot(df, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual Log"), linewidth = 0.4, alpha = 0.6) +
    geom_line(aes(y = Forecast, color = "One-step Forecast"), linetype = "dashed", linewidth = 0.6) +
    scale_color_manual(values = c("Actual Log" = "grey40", "One-step Forecast" = "firebrick")) +
    labs(
      title = title_label, 
      # Show both RMSE and MSFE formatted in non-scientific notation in the subtitle
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

# --- 4. Combine into a large plot and output ---
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
  filename = file.path(folder_name, "11. out_sample_log_comparison.png"), 
  plot = out_sample_log_comparison,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 800         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)





#===============================================================================
#--------------------7.2 Out-of-sample forecasting (log time series data, 70% training, 30% testing, multi-step forecasting)
# recursive multi-step forecasting
#===============================================================================



# 1. Extract the training set and fit ARIMA models, with differencing handled internally
# Make sure to use historical data that are only log-transformed and not differenced
train_log_cases  <- y_log_cases_full[1:split_cases]
train_log_deaths <- y_log_deaths_full[1:split_deaths]

# --- Cases models ---
cases_ar2_model  <- Arima(train_log_cases, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE)
cases_ar7_model  <- Arima(train_log_cases, order = c(7, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE)
cases_ar14_model <- Arima(train_log_cases, order = c(14, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE)

# --- Deaths models ---
deaths_ar2_model  <- Arima(train_log_deaths, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE)
deaths_ar7_model  <- Arima(train_log_deaths, order = c(7, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE)
deaths_ar14_model <- Arima(train_log_deaths, order = c(14, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), include.drift = FALSE)



# 2. Generate forecast objects with confidence intervals
forecast_horizon <- 30 # Set the time horizon for multi-step forecasting, 30 days
h_c <- forecast_horizon
h_d <- forecast_horizon

# --- Cases forecasts ---
fc_cases_ar2  <- forecast(cases_ar2_model, h = h_c, level = 95)
fc_cases_ar7  <- forecast(cases_ar7_model, h = h_c, level = 95)
fc_cases_ar14 <- forecast(cases_ar14_model, h = h_c, level = 95)

# --- Deaths forecasts ---
fc_deaths_ar2  <- forecast(deaths_ar2_model, h = h_d, level = 95)
fc_deaths_ar7  <- forecast(deaths_ar7_model, h = h_d, level = 95)
fc_deaths_ar14 <- forecast(deaths_ar14_model, h = h_d, level = 95)


# 3. Advanced visualization function with 95% intervals, adding RMSE calculation and display
plot_elegant_forecast <- function(fc_obj, full_log_series, split_idx, title) {
  # Extract forecast data
  h <- length(fc_obj$mean)
  pred_mean <- as.numeric(fc_obj$mean)
  pred_lower <- as.numeric(fc_obj$lower)
  pred_upper <- as.numeric(fc_obj$upper)
  
  # Extract the last 30 days of historical data to make the plot visually clear
  plot_start <- max(1, split_idx - 30) 
  
  # 1. Training-set data
  df_train <- data.frame(
    Time = plot_start:split_idx,
    Value = full_log_series[plot_start:split_idx]
  )
  
  # 2. True test-set data
  actual_test_values <- full_log_series[(split_idx + 1):(split_idx + h)]
  df_actual_test <- data.frame(
    Time = (split_idx + 1):(split_idx + h),
    Value = actual_test_values
  )
  
  # 3. Forecast data frame
  df_pred <- data.frame(
    Time = (split_idx + 1):(split_idx + h),
    Point = pred_mean,
    Lower = pred_lower,
    Upper = pred_upper
  )
  
  # ==========================================
  # New core code: calculate out-of-sample RMSE
  # ==========================================
  msfe <- mean((actual_test_values - pred_mean)^2, na.rm = TRUE)
  rmse <- sqrt(mean((actual_test_values - pred_mean)^2, na.rm = TRUE))
  
  # Calculate a reasonable range for the y-axis
  y_max <- max(c(df_train$Value, df_actual_test$Value, df_pred$Upper), na.rm = TRUE) * 1.05
  y_min <- max(-1, min(c(df_train$Value, df_actual_test$Value, df_pred$Lower), na.rm = TRUE))
  
  # Extract the connection point
  connect_x <- split_idx
  connect_y <- full_log_series[split_idx]
  
  # ggplot plotting
  ggplot() +
    geom_ribbon(data = df_pred, aes(x = Time, ymin = Lower, ymax = Upper, fill = "95% Prediction Interval"), alpha = 0.2) +
    geom_line(data = df_train, aes(x = Time, y = Value, color = "Training Data"), linewidth = 0.8) +
    geom_line(data = df_actual_test, aes(x = Time, y = Value, color = "Actual Test Data"), alpha = 0.6, linewidth = 0.8) +
    geom_line(data = df_pred, aes(x = Time, y = Point, color = "Out-of-sample Forecast"), linetype = "dashed", linewidth = 0.8) +
    geom_segment(aes(x = connect_x, y = connect_y, xend = split_idx + 1, yend = pred_mean[1]), 
                 color = "firebrick", linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = split_idx, linetype = "dotted", color = "grey50") +
    scale_color_manual(name = "", values = c("Training Data" = "black", "Actual Test Data" = "grey50", "Out-of-sample Forecast" = "firebrick")) +
    scale_fill_manual(name = "", values = c("95% Prediction Interval" = "firebrick")) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    
    labs(
      title = paste0(title, " | Out-of-sample RMSE: ", round(rmse, 4), 
                     " | MSFE: ", round(msfe, 4)),
      x = "Days", 
      y = "Log Level"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",          
      legend.box = "horizontal",           
      legend.margin = margin(t = -10)
    )
}
# Note: the third argument should be split_cases and split_deaths to ensure a rigorous time-axis split point
Multistep_Outsample_p1 <- plot_elegant_forecast(fc_cases_ar2,  y_log_cases_full, split_cases, "Cases AR(2)")
Multistep_Outsample_p2 <- plot_elegant_forecast(fc_cases_ar7,  y_log_cases_full, split_cases, "Cases AR(7)")
Multistep_Outsample_p3 <- plot_elegant_forecast(fc_cases_ar14, y_log_cases_full, split_cases, "Cases AR(14)")

Multistep_Outsample_p4 <- plot_elegant_forecast(fc_deaths_ar2,  y_log_deaths_full, split_deaths, "Deaths AR(2)")
Multistep_Outsample_p5 <- plot_elegant_forecast(fc_deaths_ar7,  y_log_deaths_full, split_deaths, "Deaths AR(7)")
Multistep_Outsample_p6 <- plot_elegant_forecast(fc_deaths_ar14, y_log_deaths_full, split_deaths, "Deaths AR(14)")


# integrate the plots
Multistep_Outsample_Plot <- ((Multistep_Outsample_p1 / Multistep_Outsample_p2 / Multistep_Outsample_p3) | 
                               (Multistep_Outsample_p4 / Multistep_Outsample_p5 / Multistep_Outsample_p6)) + 
  plot_annotation(
    title = "Pure AR Multi-step Forecast: 30-Day Horizon on Log Scale",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

# Print the final plot
print(Multistep_Outsample_Plot)
# save the plot
ggsave(
  filename = file.path(folder_name, "12. Multistep_Outsample_Plot.png"), 
  plot = Multistep_Outsample_Plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)














#===============================================================================
#----------------------------8. d) add Predictor
#===============================================================================

# ====================1 Build age-based predictors from the RKI dataset=============================
covid_age <- data_infektionen %>%
  mutate(
    is_60plus = grepl("^A(6|7|8|9)", Altersgruppe),
    is_80plus = grepl("^A8", Altersgruppe)
  )

daily_age_cases <- covid_age %>%
  group_by(Meldedatum) %>%
  summarise(
    cases_total = sum(AnzahlFall),
    cases_60plus = sum(AnzahlFall[is_60plus], na.rm = TRUE),
    cases_80plus = sum(AnzahlFall[is_80plus], na.rm = TRUE),
    share_cases_60plus = ifelse(cases_total > 0, cases_60plus / cases_total, 0),
    share_cases_80plus = ifelse(cases_total > 0, cases_80plus / cases_total, 0),
    .groups = "drop"
  )

daily_age_deaths <- covid_age %>%
  filter(NeuerTodesfall %in% c(0L, 1L)) %>%
  group_by(Meldedatum) %>%
  summarise(
    deaths_total = sum(AnzahlTodesfall),
    deaths_60plus = sum(AnzahlTodesfall[is_60plus], na.rm = TRUE),
    deaths_80plus = sum(AnzahlTodesfall[is_80plus], na.rm = TRUE),
    share_deaths_60plus = ifelse(deaths_total > 0, deaths_60plus / deaths_total, 0),
    share_deaths_80plus = ifelse(deaths_total > 0, deaths_80plus / deaths_total, 0),
    .groups = "drop"
  )




#=================== 2 Download Germany daily mean temperature (source: Open-Meteo)==================
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
    temp_c = zoo::na.approx(temp_c, na.rm = FALSE),
    temp_c = zoo::na.locf(temp_c, na.rm = FALSE),
    temp_c = zoo::na.locf(temp_c, fromLast = TRUE, na.rm = FALSE)
  )



#=====================3 Build the final daily predictor table============================
Sys.setlocale("LC_TIME", "English")
predictors_daily <- data.frame(Meldedatum = daily_cases$Meldedatum) %>%
  left_join(daily_temp, by = "Meldedatum") %>%
  left_join(daily_age_cases %>% select(Meldedatum, share_cases_60plus,share_cases_80plus),
            by = "Meldedatum") %>%
  left_join(daily_age_deaths %>% select(Meldedatum, share_deaths_60plus,share_deaths_80plus),
            by = "Meldedatum") %>%
  mutate(
    share_cases_60plus = replace(share_cases_60plus, is.na(share_cases_60plus), 0),
    share_cases_80plus = replace(share_cases_80plus, is.na(share_cases_80plus), 0),
    share_deaths_60plus = replace(share_deaths_60plus, is.na(share_deaths_60plus), 0),
    share_deaths_80plus = replace(share_deaths_80plus, is.na(share_deaths_80plus), 0),
    weekday = weekdays(Meldedatum),
    is_weekend = as.integer(weekday %in% c("Saturday", "Sunday")),
    time_idx = as.numeric(Meldedatum - min(Meldedatum)) + 1
  )


xreg_cases_raw  <- as.matrix(predictors_daily[, c("temp_c", "share_cases_60plus")])
xreg_deaths_raw <- as.matrix(predictors_daily[, c("temp_c", "share_deaths_80plus")])

manual_double_diff <- function(x) {
  d1 <- diff(x, differences = 1)
  d1_7 <- diff(d1, lag = 7, differences = 1)
  return(d1_7)
}

xreg_cases_diff  <- apply(xreg_cases_raw, 2, manual_double_diff) %>% round (digits = 7)
xreg_deaths_diff <- apply(xreg_deaths_raw, 2, manual_double_diff) %>% round (digits = 7)


# Split into training and test sets
n_diff <- length(cases_processed) # length after double differencing
split_idx <- floor(n_diff * 0.70) # 70% training set

# Get the training set of predictors, already differenced but not log-transformed, because temperature contains negative values and cannot be log-transformed; age proportions do not need log transformation
x_train_cases_stat  <- xreg_cases_diff[1:split_idx, , drop = FALSE]
x_train_deaths_stat <- xreg_deaths_diff[1:split_idx, , drop = FALSE]


# ==============================================================================
# -------------------------8.1 In-sample One-step Forecast
# In-sample: the higher the AR order, the better the forecasting performance
# ==============================================================================

xreg_cases_raw  <- as.matrix(predictors_daily[, c("temp_c", "share_cases_60plus")])
xreg_deaths_raw <- as.matrix(predictors_daily[, c("temp_c", "share_deaths_80plus")])


manual_double_diff <- function(x) {
  d1 <- diff(x, differences = 1)
  d1_7 <- diff(d1, lag = 7, differences = 1)
  return(d1_7)
}


xreg_cases_diff_full  <- apply(xreg_cases_raw, 2, manual_double_diff)  %>% round (digits = 7)
xreg_deaths_diff_full <- apply(xreg_deaths_raw, 2, manual_double_diff) %>% round (digits = 7)



# --- 3. Train full-sample stationary ARX models ---
cases_arx2_full  <- Arima(cases_processed, order = c(2, 0, 0), xreg = xreg_cases_diff_full, include.mean = FALSE, method = "ML")
cases_arx7_full  <- Arima(cases_processed, order = c(7, 0, 0), xreg = xreg_cases_diff_full, include.mean = FALSE, method = "ML")
cases_arx14_full <- Arima(cases_processed, order = c(14, 0, 0), xreg = xreg_cases_diff_full, include.mean = FALSE, method = "ML")

deaths_arx2_full  <- Arima(deaths_processed, order = c(2, 0, 0), xreg = xreg_deaths_diff_full, include.mean = FALSE, method = "ML")
deaths_arx7_full  <- Arima(deaths_processed, order = c(7, 0, 0), xreg = xreg_deaths_diff_full, include.mean = FALSE, method = "ML")
deaths_arx14_full <- Arima(deaths_processed, order = c(14, 0, 0), xreg = xreg_deaths_diff_full, include.mean = FALSE, method = "ML")


# --- 4. In-sample one-step forecast reconstruction function ---
arx_insample_onestep <- function(model_stat, raw_series, p_label) {
  # a. Extract the Log series of the original data, length 2191
  y_log <- log1p(as.numeric(raw_series))
  n <- length(y_log)
  
  # b. Extract fitted values of the model in the stationary state; this is the one-step prediction \hat{z}_t including the effect of external variables X*
  z_hat <- as.numeric(fitted(model_stat))
  
  # c. Strictly align indices: differencing loses 8 points, so forecasting starts from the 9th day
  idx_target <- 9:n
  
  # Extract true historical lagged terms
  lag1 <- y_log[idx_target - 1]
  lag7 <- y_log[idx_target - 7]
  lag8 <- y_log[idx_target - 8]
  
  # d. Perfect inverse reconstruction of integration, at the Log-level forecast
  pred_log <- z_hat + lag1 + lag7 - lag8
  actual_log <- y_log[idx_target]
  
  # e. Calculate errors
  msfe <- mean((actual_log - pred_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # f. Prepare plotting data, taking the last 500 days so that the fit is clearly visible; plotting the full sample would be too cluttered
  plot_start <- n - 500 
  idx_plot <- plot_start:n
  
  df_plot <- data.frame(
    Day = idx_plot,
    Actual = y_log[idx_plot],
    Predicted = pred_log[(plot_start - 8):(n - 8)] # Align the offset
  )
  
  # g. Plot
  p <- ggplot(df_plot, aes(x = Day)) +
    geom_line(aes(y = Actual, color = "Actual Log Data"), alpha = 0.6, linewidth = 0.8) +
    geom_line(aes(y = Predicted, color = "One-Step ARX Forecast"), linetype = "dashed", linewidth = 0.8) +
    scale_color_manual(name = "", values = c("Actual Log Data" = "black", "One-Step ARX Forecast" = "firebrick")) +
    labs(title = paste0("ARX(", p_label, ") One-Step In-sample | RMSE: ", round(rmse, 4) ," | MSFE: ", format(msfe, scientific = FALSE, digits = 4)),
         
         y = "Log Level", x = "Days") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}

# --- 5. Execute forecasting ---
# APISOSF = add_predictor_in_sample_one_step_forcast
APISOSF_cases_res2  <- arx_insample_onestep(cases_arx2_full,  daily_cases$new_cases, "2")
APISOSF_cases_res7  <- arx_insample_onestep(cases_arx7_full,  daily_cases$new_cases, "7")
APISOSF_cases_res14 <- arx_insample_onestep(cases_arx14_full, daily_cases$new_cases, "14")

APISOSF_deaths_res2  <- arx_insample_onestep(deaths_arx2_full,  daily_deaths$deaths, "2")
APISOSF_deaths_res7  <- arx_insample_onestep(deaths_arx7_full,  daily_deaths$deaths, "7")
APISOSF_deaths_res14 <- arx_insample_onestep(deaths_arx14_full, daily_deaths$deaths, "14")

# --- 6. Combine and output the large plot ---
APISOSF_cases_plots <- (APISOSF_cases_res2$plot / APISOSF_cases_res7$plot / APISOSF_cases_res14$plot) +
  plot_annotation(title = "Cases: Full-Sample One-Step ARX Forecast",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

APISOSF_deaths_plots <- (APISOSF_deaths_res2$plot / APISOSF_deaths_res7$plot / APISOSF_deaths_res14$plot) +
  plot_annotation(title = "Deaths: Full-Sample One-Step ARX Forecast",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

final_APISOSF_plot <- wrap_elements(APISOSF_cases_plots) | wrap_elements(APISOSF_deaths_plots)
print(final_APISOSF_plot)


# save the plot
ggsave(
  filename = file.path(folder_name, "13. Add_Predictor_One_Step_In_sample_Plot.png"), 
  plot = final_APISOSF_plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)





# ==============================================================================
# -------------------------8.2 In-sample Multi-step Forecast
# In-sample: the higher the AR order, the better the forecasting performance
# ==============================================================================

# --- 1. Prepare the original data and external-variable matrices, using the complete 2191-row data without manual differencing ---
# Note: here we extract the absolute raw proportions and temperature, without differencing; Arima() handles differencing internally
xreg_cases_raw  <- as.matrix(predictors_daily[, c("temp_c", "share_cases_60plus")])
xreg_deaths_raw <- as.matrix(predictors_daily[, c("temp_c", "share_deaths_80plus")])

n_total <- length(cases_log_processed)
split_idx <- floor(n_total * 0.70) 
h_horizon <- n_total - split_idx  # Step length of the final 30%, approximately 658 days

# --- 2. Train full-sample, 100%, ARX models with internal differencing ---
cat("Training 100% Full-Sample ARX Models (with internal differencing)...\n")
cases_arx2_100  <- Arima(cases_log_processed, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), xreg = xreg_cases_raw, include.drift = FALSE, method = "ML")
cases_arx7_100  <- Arima(cases_log_processed, order = c(7, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), xreg = xreg_cases_raw, include.drift = FALSE, method = "ML")
cases_arx14_100 <- Arima(cases_log_processed, order = c(14, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), xreg = xreg_cases_raw, include.drift = FALSE, method = "ML")

deaths_arx2_100  <- Arima(deaths_log_processed, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), xreg = xreg_deaths_raw, include.drift = FALSE, method = "ML")
deaths_arx7_100  <- Arima(deaths_log_processed, order = c(7, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), xreg = xreg_deaths_raw, include.drift = FALSE, method = "ML")
deaths_arx14_100 <- Arima(deaths_log_processed, order = c(14, 1, 0), seasonal = list(order = c(0, 1, 0), period = 7), xreg = xreg_deaths_raw, include.drift = FALSE, method = "ML")

# --- 3. Dynamic historical simulation function, truncated to 70% and extrapolated for 30% ---
arx_dynamic_forecast_30d <- function(model_100, raw_series, xreg_full, p_label, h = 30) {
  
  y_log <- as.numeric(raw_series)
  n_total <- length(y_log)
  h_full <- n_total - split_idx  # Complete 30% test-set length
  
  # a. Truncate historical data and external variables, from 0% to the 70% split point
  y_log_trunc <- y_log[1:split_idx]
  xreg_trunc  <- xreg_full[1:split_idx, , drop = FALSE]
  
  # b. Precisely extract the complete future external variables; to calculate the full-sample error, all future values must be extracted
  xreg_future_full <- xreg_full[(split_idx + 1):n_total, , drop = FALSE]
  
  # c. Force the parameters trained on the full sample to be applied to the truncated historical data
  historical_model <- Arima(y_log_trunc, model = model_100, xreg = xreg_trunc)
  
  # d. Dynamically extrapolate the complete 30% forecast, forecasting h_full days
  fc_full <- forecast(historical_model, h = h_full, xreg = xreg_future_full, level = 95)
  
  preds_log_full <- as.numeric(fc_full$mean)
  lower_log_full <- as.numeric(fc_full$lower)
  upper_log_full <- as.numeric(fc_full$upper)
  
  # e. ==========================================
  #    Core modification: calculate the true MSFE/RMSE on the complete 30% test set
  # ==========================================
  actual_log_future_full <- y_log[(split_idx + 1):n_total]
  msfe <- mean((actual_log_future_full - preds_log_full)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # f. ==========================================
  #    Visual truncation: extract only the first h = 30 days for plotting
  # ==========================================
  plot_start <- split_idx - 70 + 1  # Ensure that the historical data are exactly 70 days
  
  df_train <- data.frame(
    Day = plot_start:split_idx, 
    Value = y_log[plot_start:split_idx]
  )
  
  df_actual_test <- data.frame(
    Day = (split_idx + 1):(split_idx + h), 
    Value = actual_log_future_full[1:h] # Use only the first 30 days of true data
  )
  
  df_pred <- data.frame(
    Day = (split_idx + 1):(split_idx + h), 
    Point = preds_log_full[1:h],        # Use only the first 30 days of predicted values
    Lower = lower_log_full[1:h], 
    Upper = upper_log_full[1:h]
  )
  
  # Calculate y-axis range to prevent compression, only based on the plotting region
  y_max <- max(c(df_train$Value, df_actual_test$Value, df_pred$Upper), na.rm = TRUE) * 1.05
  y_min <- max(-1, min(c(df_train$Value, df_actual_test$Value, df_pred$Lower), na.rm = TRUE))
  
  # g. Advanced ggplot visualization
  p <- ggplot() +
    geom_ribbon(data = df_pred, aes(x = Day, ymin = Lower, ymax = Upper, fill = "95% Prediction Interval"), alpha = 0.2) +
    geom_line(data = df_train, aes(x = Day, y = Value, color = "70 Days History"), linewidth = 0.6) +
    geom_line(data = df_actual_test, aes(x = Day, y = Value, color = "Actual 30 Days"), alpha = 0.4, linewidth = 0.6) +
    geom_line(data = df_pred, aes(x = Day, y = Point, color = "Dynamic ARX Forecast"), linetype = "dashed", linewidth = 0.8) +
    
    # Connection line, stitching the history and the forecast
    geom_segment(aes(x = split_idx, y = y_log[split_idx], xend = split_idx + 1, yend = preds_log_full[1]), 
                 color = "firebrick", linetype = "dashed", linewidth = 0.8) +
    
    geom_vline(xintercept = split_idx, linetype = "dotted", color = "grey50") +
    
    scale_color_manual(name = "", values = c("70 Days History" = "black", "Actual 30 Days" = "grey30", "Dynamic ARX Forecast" = "firebrick")) +
    scale_fill_manual(name = "", values = c("95% Prediction Interval" = "firebrick")) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    
    # Clearly indicate in the title that the calculation is based on the full sample
    labs(title = paste0("ARX(", p_label, ") 30-Day Dynamic Forecast | RMSE: ", round(rmse, 4)),
         subtitle = paste0("MSFE: ", format(msfe, scientific = FALSE, digits = 4), " (Evaluated on full 30% horizon)"),
         y = "Log Level", x = "Days") +
    
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(size = 10, face = "bold"))
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}

# --- 4. Generate forecasts and plots ---
cat("Running 30-day zoomed dynamic forecasts...\n")
cases_dyn_arx2  <- arx_dynamic_forecast_30d(cases_arx2_100, cases_log_processed, xreg_cases_raw, "2", h = 30)
cases_dyn_arx7  <- arx_dynamic_forecast_30d(cases_arx7_100, cases_log_processed, xreg_cases_raw, "7", h = 30)
cases_dyn_arx14 <- arx_dynamic_forecast_30d(cases_arx14_100, cases_log_processed, xreg_cases_raw, "14", h = 30)

deaths_dyn_arx2  <- arx_dynamic_forecast_30d(deaths_arx2_100, deaths_log_processed, xreg_deaths_raw, "2", h = 30)
deaths_dyn_arx7  <- arx_dynamic_forecast_30d(deaths_arx7_100, deaths_log_processed, xreg_deaths_raw, "7", h = 30)
deaths_dyn_arx14 <- arx_dynamic_forecast_30d(deaths_arx14_100, deaths_log_processed, xreg_deaths_raw, "14", h = 30)

# --- 5. Combine and print the large plot ---
cases_plots <- (cases_dyn_arx2$plot / cases_dyn_arx7$plot / cases_dyn_arx14$plot) +
  plot_annotation(title = "Cases: 30-Day Pseudo-Out-of-Sample Dynamic Forecast", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

deaths_plots <- (deaths_dyn_arx2$plot / deaths_dyn_arx7$plot / deaths_dyn_arx14$plot) +
  plot_annotation(title = "Deaths: 30-Day Pseudo-Out-of-Sample Dynamic Forecast", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

final_30d_plot <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(final_30d_plot)


# save the plot
ggsave(
  filename = file.path(folder_name, "14. Add_Predictor_MultiStep_InSample_Plot.png"), 
  plot = final_30d_plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)








# ==============================================================================
# 8.3 Out-of-sample One-step Forecasting
# ==============================================================================

# Core forecast reconstruction function
arx_outsample_onestep <- function(train_model_stat, raw_series, stat_series, xreg_diff_full, split_idx, p_label) {
  
  y_log <- log1p(as.numeric(raw_series))
  n_log <- length(y_log)
  n_stat <- length(stat_series)
  
  # ======================================================
  # Safety check: stationary y and stationary xreg must have the same length
  # ======================================================
  if (n_stat != nrow(xreg_diff_full)) {
    stop(
      paste0(
        "Length mismatch inside arx_outsample_onestep: ",
        "length(stat_series) = ", n_stat,
        ", nrow(xreg_diff_full) = ", nrow(xreg_diff_full)
      )
    )
  }
  
  if (split_idx >= n_stat) {
    stop(
      paste0(
        "split_idx is too large: split_idx = ", split_idx,
        ", n_stat = ", n_stat
      )
    )
  }
  
  # a. Extract the stationary data and stationary external variables for the 30% test set
  y_test_stat <- stat_series[(split_idx + 1):n_stat]
  x_test_stat <- xreg_diff_full[(split_idx + 1):n_stat, , drop = FALSE]
  
  # Check the test-set length again
  if (length(y_test_stat) != nrow(x_test_stat)) {
    stop(
      paste0(
        "Test set length mismatch: ",
        "length(y_test_stat) = ", length(y_test_stat),
        ", nrow(x_test_stat) = ", nrow(x_test_stat)
      )
    )
  }
  
  test_model <- Arima(
    y_test_stat,
    model = train_model_stat,
    xreg = x_test_stat
  )
  
  z_hat_out <- as.numeric(fitted(test_model))
  
  idx_target <- (split_idx + 1 + 8):(n_stat + 8)
  
  lag1 <- y_log[idx_target - 1]
  lag7 <- y_log[idx_target - 7]
  lag8 <- y_log[idx_target - 8]
  
  pred_log_out <- z_hat_out + lag1 + lag7 - lag8
  actual_log_out <- y_log[idx_target]
  
  msfe <- mean((actual_log_out - pred_log_out)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  plot_start <- max(1, split_idx + 8 - 1520)
  
  df_train <- data.frame(
    Day = plot_start:(split_idx + 8),
    Value = y_log[plot_start:(split_idx + 8)]
  )
  
  df_actual_test <- data.frame(
    Day = idx_target,
    Value = actual_log_out
  )
  
  df_pred <- data.frame(
    Day = idx_target,
    Point = pred_log_out
  )
  
  y_max <- max(c(df_train$Value, df_actual_test$Value, df_pred$Point), na.rm = TRUE) * 1.05
  y_min <- min(c(df_train$Value, df_actual_test$Value, df_pred$Point), na.rm = TRUE) * 0.95
  
  p <- ggplot() +
    geom_line(
      data = df_train,
      aes(x = Day, y = Value, color = "70% Training Data"),
      linewidth = 0.6
    ) +
    geom_line(
      data = df_actual_test,
      aes(x = Day, y = Value, color = "30% Actual Test Data"),
      alpha = 0.5,
      linewidth = 0.8
    ) +
    geom_line(
      data = df_pred,
      aes(x = Day, y = Point, color = "One-Step ARX Forecast"),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = split_idx + 8,
      linetype = "dotted",
      color = "grey"
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "70% Training Data" = "grey",
        "30% Actual Test Data" = "grey50",
        "One-Step ARX Forecast" = "firebrick"
      )
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(
      title = paste0(
        "ARX(", p_label, ") Out-of-sample One-Step | RMSE: ",
        round(rmse, 4),
        " | MSFE: ",
        format(msfe, scientific = FALSE, digits = 4)
      ),
      y = "Log Level",
      x = "Days"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}


# ==========================================================
# Redefine the training-set split point used by ARX
# Note: this must be based on the length of the processed series, not the length of the original log-level series
# ==========================================================

split_cases_stat  <- floor(length(cases_processed) * 0.70)
split_deaths_stat <- floor(length(deaths_processed) * 0.70)

# ==========================================================
# Check whether y and xreg have the same length
# ==========================================================

cat("Cases processed length:", length(cases_processed), "\n")
cat("Cases xreg rows:", nrow(xreg_cases_diff), "\n")

cat("Deaths processed length:", length(deaths_processed), "\n")
cat("Deaths xreg rows:", nrow(xreg_deaths_diff), "\n")

if (length(cases_processed) != nrow(xreg_cases_diff)) {
  stop("Cases: length(cases_processed) != nrow(xreg_cases_diff)")
}

if (length(deaths_processed) != nrow(xreg_deaths_diff)) {
  stop("Deaths: length(deaths_processed) != nrow(xreg_deaths_diff)")
}

# ==========================================================
# Split the training set
# ==========================================================

train_cases_stat  <- cases_processed[1:split_cases_stat]
train_deaths_stat <- deaths_processed[1:split_deaths_stat]

x_train_cases_stat <- xreg_cases_diff[1:split_cases_stat, , drop = FALSE]
x_train_deaths_stat <- xreg_deaths_diff[1:split_deaths_stat, , drop = FALSE]

# Check the training-set length again
cat("length(train_cases_stat):", length(train_cases_stat), "\n")
cat("nrow(x_train_cases_stat):", nrow(x_train_cases_stat), "\n")

cat("length(train_deaths_stat):", length(train_deaths_stat), "\n")
cat("nrow(x_train_deaths_stat):", nrow(x_train_deaths_stat), "\n")

if (length(train_cases_stat) != nrow(x_train_cases_stat)) {
  stop("Cases training data: y and xreg length mismatch")
}

if (length(train_deaths_stat) != nrow(x_train_deaths_stat)) {
  stop("Deaths training data: y and xreg length mismatch")
}

# ==========================================================
# Train ARX models
# ==========================================================

cases_arx2_stat  <- Arima(train_cases_stat, order = c(2, 0, 0), xreg = x_train_cases_stat, include.mean = FALSE, method = "ML")
cases_arx7_stat  <- Arima(train_cases_stat, order = c(7, 0, 0), xreg = x_train_cases_stat, include.mean = FALSE, method = "ML")
cases_arx14_stat <- Arima(train_cases_stat, order = c(14, 0, 0), xreg = x_train_cases_stat, include.mean = FALSE, method = "ML")

deaths_arx2_stat  <- Arima(train_deaths_stat, order = c(2, 0, 0), xreg = x_train_deaths_stat, include.mean = FALSE, method = "ML")
deaths_arx7_stat  <- Arima(train_deaths_stat, order = c(7, 0, 0), xreg = x_train_deaths_stat, include.mean = FALSE, method = "ML")
deaths_arx14_stat <- Arima(train_deaths_stat, order = c(14, 0, 0), xreg = x_train_deaths_stat, include.mean = FALSE, method = "ML")

cases_out_onestep2   <- arx_outsample_onestep(cases_arx2_stat, daily_cases$new_cases, cases_processed, xreg_cases_diff, split_cases_stat, "2")
cases_out_onestep7   <- arx_outsample_onestep(cases_arx7_stat, daily_cases$new_cases, cases_processed, xreg_cases_diff, split_cases_stat, "7")
cases_out_onestep14  <- arx_outsample_onestep(cases_arx14_stat, daily_cases$new_cases, cases_processed, xreg_cases_diff, split_cases_stat, "14")

deaths_out_onestep2  <- arx_outsample_onestep(deaths_arx2_stat, daily_deaths$deaths, deaths_processed, xreg_deaths_diff, split_deaths_stat, "2")
deaths_out_onestep7  <- arx_outsample_onestep(deaths_arx7_stat, daily_deaths$deaths, deaths_processed, xreg_deaths_diff, split_deaths_stat, "7")
deaths_out_onestep14 <- arx_outsample_onestep(deaths_arx14_stat, daily_deaths$deaths, deaths_processed, xreg_deaths_diff, split_deaths_stat, "14")

# --- Combine and print the large plot ---
cases_plots <- (cases_out_onestep2$plot / cases_out_onestep7$plot / cases_out_onestep14$plot) +
  plot_annotation(title = "Cases: Add-Predictor Out-of-Sample One-Step Forecast", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

deaths_plots <- (deaths_out_onestep2$plot / deaths_out_onestep7$plot / deaths_out_onestep14$plot) +
  plot_annotation(title = "Deaths: Add-Predictor Out-of-Sample One-Step Forecast", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

final_outsample_onestep_plot <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(final_outsample_onestep_plot)



# save the plot
ggsave(
  filename = file.path(folder_name, "15. Add_Predictor_OneStep_OutofSample_Plot.png"), 
  plot = final_outsample_onestep_plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Note: 300-600 DPI is already very clear and keeps file size moderate; 1000 may lead to a very large file
)










# ==============================================================================
# 8.4 Out-of-sample Multi-step Forecasting
# ==============================================================================

# Extract the original Log series and the undifferenced predictor matrices
xreg_cases_raw  <- as.matrix(predictors_daily[, c("temp_c", "share_cases_60plus")])
xreg_deaths_raw <- as.matrix(predictors_daily[, c("temp_c", "share_deaths_80plus")])

n_total <- length(cases_log_processed)
split_idx <- floor(n_total * 0.70) 

# --- Core forecasting function ---
arx_outsample_multistep <- function(y_log_full, xreg_full, split_idx, p_val, plot_h = 60) {
  
  # 1. Strictly split the training set and test set
  y_train <- y_log_full[1:split_idx]
  x_train <- xreg_full[1:split_idx, , drop = FALSE]
  
  y_test <- y_log_full[(split_idx + 1):n_total]
  x_test <- xreg_full[(split_idx + 1):n_total, , drop = FALSE]
  
  n_test <- length(y_test) # Complete 30% step length, approximately 658 days
  
  # 2. Train the ARX model only on the 70% training set
  # The model internally includes d = 1 and D = 1, ensuring accurate derivation of prediction intervals
  fit_train <- Arima(y_train, order = c(p_val, 1, 0), 
                     seasonal = list(order = c(0, 1, 0), period = 7), 
                     xreg = x_train, include.drift = FALSE, method = "ML")
  
  # 3. Blind out-of-sample extrapolation, forecasting the complete 30% test set
  # At this point, the model does not know anything about y_test and relies only on x_test
  fc <- forecast(fit_train, h = n_test, xreg = x_test, level = 95)
  
  preds_log <- as.numeric(fc$mean)
  lower_log <- as.numeric(fc$lower)
  upper_log <- as.numeric(fc$upper)
  
  # 4. Calculate the true MSFE / RMSE on the complete 30% test set
  msfe <- mean((y_test - preds_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # ==========================================================
  # 5. Prepare plotting data, using a zoomed view to avoid compression: 70 historical days + first 30 forecast days
  # ==========================================================
  plot_start <- split_idx - 70 + 1
  plot_end_test <- split_idx + plot_h
  
  df_train <- data.frame(
    Day = plot_start:split_idx,
    Value = y_log_full[plot_start:split_idx]
  )
  
  df_actual_test <- data.frame(
    Day = (split_idx + 1):plot_end_test,
    Value = y_test[1:plot_h]
  )
  
  df_pred <- data.frame(
    Day = (split_idx + 1):plot_end_test,
    Point = preds_log[1:plot_h],
    Lower = lower_log[1:plot_h],
    Upper = upper_log[1:plot_h]
  )
  
  # Dynamically calculate the y-axis range to prevent compression caused by the confidence interval
  y_max <- max(c(df_train$Value, df_actual_test$Value, df_pred$Upper), na.rm = TRUE) * 1.05
  y_min <- max(-1, min(c(df_train$Value, df_actual_test$Value, df_pred$Lower), na.rm = TRUE))
  
  # 6. ggplot visualization
  p <- ggplot() +
    geom_ribbon(data = df_pred, aes(x = Day, ymin = Lower, ymax = Upper, fill = "95% Prediction Interval"), alpha = 0.2) +
    geom_line(data = df_train, aes(x = Day, y = Value, color = "70% Training Data"), linewidth = 0.6) +
    geom_line(data = df_actual_test, aes(x = Day, y = Value, color = "Actual Test Data"), alpha = 0.5, linewidth = 0.8) +
    geom_line(data = df_pred, aes(x = Day, y = Point, color = "Out-of-sample Forecast"), linetype = "dashed", linewidth = 0.8) +
    
    # Perfect stitching line
    geom_segment(aes(x = split_idx, y = y_log_full[split_idx], xend = split_idx + 1, yend = preds_log[1]), 
                 color = "firebrick", linetype = "dashed", linewidth = 0.8) +
    
    geom_vline(xintercept = split_idx, linetype = "dotted", color = "grey50") +
    scale_color_manual(name = "", values = c("70% Training Data" = "black", "Actual Test Data" = "grey40", "Out-of-sample Forecast" = "firebrick")) +
    scale_fill_manual(name = "", values = c("95% Prediction Interval" = "firebrick")) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    
    labs(title = paste0("ARX(", p_val, ") True Out-of-Sample | RMSE: ", round(rmse, 4)),
         subtitle = paste0("MSFE: ", format(msfe, scientific = FALSE, digits = 4), " (Evaluated on full 30% horizon)"),
         y = "Log Level", x = "Days") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(size = 10, face = "bold"))
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}

# --- Execute out-of-sample multi-step forecasting ---
cat("Running true out-of-sample multi-step forecasts...\n")
cases_out_multi2  <- arx_outsample_multistep(cases_log_processed, xreg_cases_raw, split_idx, 2, plot_h = 30)
cases_out_multi7  <- arx_outsample_multistep(cases_log_processed, xreg_cases_raw, split_idx, 7, plot_h = 30)
cases_out_multi14 <- arx_outsample_multistep(cases_log_processed, xreg_cases_raw, split_idx, 14, plot_h = 30)

deaths_out_multi2  <- arx_outsample_multistep(deaths_log_processed, xreg_deaths_raw, split_idx, 2, plot_h = 30)
deaths_out_multi7  <- arx_outsample_multistep(deaths_log_processed, xreg_deaths_raw, split_idx, 7, plot_h = 30)
deaths_out_multi14 <- arx_outsample_multistep(deaths_log_processed, xreg_deaths_raw, split_idx, 14, plot_h = 30)

# --- Combine and print the large plot ---
cases_plots <- (cases_out_multi2$plot / cases_out_multi7$plot / cases_out_multi14$plot) +
  plot_annotation(title = "Cases: True Out-of-Sample Multi-step Forecast (First 60 Days Zoom)", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

deaths_plots <- (deaths_out_multi2$plot / deaths_out_multi7$plot / deaths_out_multi14$plot) +
  plot_annotation(title = "Deaths: True Out-of-Sample Multi-step Forecast (First 60 Days Zoom)", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

final_outsample_multi_plot <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(final_outsample_multi_plot)


# save the plot
ggsave(
  filename = file.path(folder_name, "16. Add_Predictor_MultiStep_OutofSample_Plot.png"), 
  plot = final_outsample_multi_plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Resolution
)













# ==============================================================================
# 9. e) SARIMA with Annual Lag (without extra predictors)
# ==============================================================================
# Basic setup: extract the original Log data and stationary data, already first-differenced and seasonally differenced at lag 7
y_log_cases <- cases_log_processed
y_stat_cases <- cases_processed

y_log_deaths <- deaths_log_processed
y_stat_deaths <- deaths_processed

n_stat <- length(y_stat_cases)
split_idx <- floor(n_stat * 0.70) 
forecast_h <- 30



# --- Core: OLS dynamic forecast reconstruction function that bypasses lag restrictions ---
run_annual_sarima_forecast <- function(y_stat, y_log, p_val, title_prefix) {
  
  # 1. Extract the training set of the stationary data
  y_train_stat <- y_stat[1:split_idx]
  
  # 2. Core workaround: manually construct the lag matrix for OLS
  # The maximum number of days to trace back is 365 + p_val
  max_lag <- 365 + p_val
  n_train <- length(y_train_stat)
  
  # Target variable, dropping the first max_lag observations because there is not enough historical data
  y_target <- y_train_stat[(max_lag + 1):n_train]
  df_train <- data.frame(y = y_target)
  
  # Short-term lag terms, from lag_1 to lag_p
  for(i in 1:p_val) df_train[[paste0("lag_", i)]] <- y_train_stat[(max_lag + 1 - i):(n_train - i)]
  # Annual lag term, lag_365
  df_train[["lag_365"]] <- y_train_stat[(max_lag + 1 - 365):(n_train - 365)]
  # Cross-lag terms, from lag_366 to lag_365+p
  for(i in 1:p_val) df_train[[paste0("lag_365_", i)]] <- y_train_stat[(max_lag + 1 - 365 - i):(n_train - 365 - i)]
  
  # 3. Fit the OLS model, without intercept, equivalent to method="CSS"
  cat(sprintf("Training %s SARIMA(%d,0,0)(1,0,0)[365] via OLS workaround...\n", title_prefix, p_val))
  fit_lm <- lm(y ~ . - 1, data = df_train)
  
  # 4. Dynamically simulate future 30-day stationary values, z_hat
  z_pred <- numeric(forecast_h)
  current_y_stat <- y_train_stat  # Rolling pool used for simulation
  
  for(i in 1:forecast_h) {
    n_curr <- length(current_y_stat)
    
    # Construct all historical data needed for the new day
    new_row <- list()
    for(j in 1:p_val) new_row[[paste0("lag_", j)]] <- current_y_stat[n_curr - j + 1]
    new_row[["lag_365"]] <- current_y_stat[n_curr - 365 + 1]
    for(j in 1:p_val) new_row[[paste0("lag_365_", j)]] <- current_y_stat[n_curr - 365 - j + 1]
    
    # Predict the value for the next day and put it into the rolling pool for the next step
    next_z <- predict(fit_lm, newdata = as.data.frame(new_row))
    z_pred[i] <- next_z
    current_y_stat <- c(current_y_stat, next_z)
  }
  
  # 5. Dynamic recursive integration: transform the stationary predicted values back to the Log scale
  log_history_end_idx <- split_idx + 8
  pred_log <- numeric(forecast_h)
  current_log_history <- y_log[1:log_history_end_idx]
  
  for (i in 1:forecast_h) {
    curr_n <- length(current_log_history)
    next_log <- z_pred[i] + current_log_history[curr_n] + current_log_history[curr_n - 6] - current_log_history[curr_n - 7]
    pred_log[i] <- next_log
    current_log_history <- c(current_log_history, next_log)
  }
  
  # 6. Calculate final errors
  actual_log_future <- y_log[(log_history_end_idx + 1):(log_history_end_idx + forecast_h)]
  msfe <- mean((actual_log_future - pred_log)^2, na.rm = TRUE)
  rmse <- sqrt(msfe)
  
  # 7. Prepare plotting data and visualization, same as before
  plot_start <- log_history_end_idx - 70 + 1
  df_hist <- data.frame(Day = plot_start:log_history_end_idx, Value = y_log[plot_start:log_history_end_idx])
  df_actual <- data.frame(Day = (log_history_end_idx + 1):(log_history_end_idx + forecast_h), Value = actual_log_future)
  df_pred <- data.frame(Day = (log_history_end_idx + 1):(log_history_end_idx + forecast_h), Point = pred_log)
  
  y_max <- max(c(df_hist$Value, df_actual$Value, df_pred$Point), na.rm = TRUE) * 1.05
  y_min <- max(-1, min(c(df_hist$Value, df_actual$Value, df_pred$Point), na.rm = TRUE))
  
  p <- ggplot() +
    geom_line(data = df_hist, aes(x = Day, y = Value, color = "70 Days History"), linewidth = 0.6) +
    geom_line(data = df_actual, aes(x = Day, y = Value, color = "Actual 30 Days"), alpha = 0.4, linewidth = 0.8) +
    geom_line(data = df_pred, aes(x = Day, y = Point, color = "SARIMA Forecast"), linetype = "dashed", linewidth = 0.8) +
    geom_segment(aes(x = log_history_end_idx, y = y_log[log_history_end_idx], 
                     xend = log_history_end_idx + 1, yend = pred_log[1]), 
                 color = "firebrick", linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = log_history_end_idx, linetype = "dotted", color = "grey50") +
    scale_color_manual(name = "", values = c("70 Days History" = "black", "Actual 30 Days" = "grey40", "SARIMA Forecast" = "firebrick")) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(title = sprintf("%s SARIMA(%d,0,0)(1,0,0)[365] | RMSE: %.4f", title_prefix, p_val, rmse),
         subtitle = sprintf("MSFE: %.4f (Out-of-sample 30-day horizon)", msfe),
         y = "Log Level", x = "Days") +
    theme_minimal() +
    theme(legend.position = "bottom", plot.title = element_text(size = 10, face = "bold"))
  
  return(list(msfe = msfe, rmse = rmse, plot = p))
}




# --- Execute forecasting ---
cat("\nStarting Cases predictions...\n")
cases_sarima1  <- run_annual_sarima_forecast(y_stat_cases, y_log_cases, p_val = 1, title_prefix = "Cases")
cases_sarima2  <- run_annual_sarima_forecast(y_stat_cases, y_log_cases, p_val = 2, title_prefix = "Cases")
cases_sarima7  <- run_annual_sarima_forecast(y_stat_cases, y_log_cases, p_val = 7, title_prefix = "Cases")
cases_sarima14 <- run_annual_sarima_forecast(y_stat_cases, y_log_cases, p_val = 14, title_prefix = "Cases")

cat("\nStarting Deaths predictions...\n")
deaths_sarima1  <- run_annual_sarima_forecast(y_stat_deaths, y_log_deaths, p_val = 1, title_prefix = "Deaths")
deaths_sarima2  <- run_annual_sarima_forecast(y_stat_deaths, y_log_deaths, p_val = 2, title_prefix = "Deaths")
deaths_sarima7  <- run_annual_sarima_forecast(y_stat_deaths, y_log_deaths, p_val = 7, title_prefix = "Deaths")
deaths_sarima14 <- run_annual_sarima_forecast(y_stat_deaths, y_log_deaths, p_val = 14, title_prefix = "Deaths")




# --- Combine and output plots ---
cases_plots <- (cases_sarima1$plot / cases_sarima2$plot / cases_sarima7$plot / cases_sarima14$plot) +
  plot_annotation(title = "Cases: SARIMA Annual Lag Forecast (h=30)", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

deaths_plots <- (deaths_sarima1$plot / deaths_sarima2$plot / deaths_sarima7$plot / deaths_sarima14$plot) +
  plot_annotation(title = "Deaths: SARIMA Annual Lag Forecast (h=30)", theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))

final_sarima_plot <- wrap_elements(cases_plots) | wrap_elements(deaths_plots)
print(final_sarima_plot)


# save the plot
ggsave(
  filename = file.path(folder_name, "17. Annual_MultiStep_OutofSample_Plot.png"), 
  plot = final_sarima_plot,
  width = 18,       # Increase width to accommodate the two left-right columns
  height = 10,      # Significantly increase height to accommodate the three models arranged vertically
  dpi = 600         # Resolution
)


















































































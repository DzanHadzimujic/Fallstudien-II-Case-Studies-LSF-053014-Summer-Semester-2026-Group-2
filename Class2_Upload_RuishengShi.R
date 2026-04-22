library(dplyr)
library(knitr) #latex


##==============================================================================
##============================= 1. Read Data ===================================
##==============================================================================

# Read infection data
data_infektionen <- read.csv("...")
dim(data_infektionen) # Check dimensions: 7,757,192 rows and 12 columns
View(data_infektionen)
# print(colnames(data_infektionen)) # Print all column names

# Data processing for the infection table
# 1. Create a new table and extract State ID (BundeslandId) as a new column via integer division
data_infektionen_new <- data_infektionen
data_infektionen_new$BundeslandId_Impfort <- data_infektionen_new$IdLandkreis %/% 1000

# 2. Relocate the BundeslandId_Impfort column to the second position
data_infektionen_new <- data_infektionen_new %>%
  relocate(BundeslandId_Impfort, .after = IdLandkreis)

# 3. Inspect the BundeslandId_Impfort column to verify successful extraction
table(data_infektionen_new$BundeslandId_Impfort)

# 4. Map and add the corresponding State names
# Step 1: Extract a unique "ID-StateName" mapping table (dictionary) from the immunization table
data_immunizations_germany <- read.csv(r"(C:\Users\RAY\OneDrive\Desktop\...)")
bundesland_map  <- data_immunizations_germany %>%
  select(BundeslandId_Impfort, Bundesland) %>%
  distinct() # distinct() removes duplicate rows to ensure a 1:1 mapping for each ID

# Step 2: Merge the mapping table into the infection data
data_infektionen_new <- data_infektionen_new %>%
  left_join(bundesland_map , by = "BundeslandId_Impfort") %>%
  relocate(Bundesland, .after = BundeslandId_Impfort) # Move State Name next to the ID for easier viewing
# print(colnames(data_infektionen_new)) # Print all column names

# Check distribution of values for specific columns
table(data_infektionen_new$NeuerFall)
table(data_infektionen_new$NeuerTodesfall)
table(data_infektionen_new$NeuGenesen)




# -----------------------------
# 1. Load required packages
# -----------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------
# 2. Convert date column to Date type
# -----------------------------
data_infektionen_new <- data_infektionen_new %>%
  mutate(
    Meldedatum = as.Date(Meldedatum)
  )

# -----------------------------
# 3. Calculate daily new cases 
#    based on Meldedatum
# -----------------------------
daily_cases <- data_infektionen_new %>%
  group_by(Meldedatum) %>%
  summarise(
    new_cases = sum(AnzahlFall, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 4. Fill in missing dates from 
#    start to end; set missing values to 0
# -----------------------------
all_dates <- data.frame(
  Meldedatum = seq(min(daily_cases$Meldedatum),
                   max(daily_cases$Meldedatum),
                   by = "day")
)

daily_cases_full <- all_dates %>%                    ############################ Time series plot of new infections
  left_join(daily_cases, by = "Meldedatum") %>%
  mutate(
    new_cases = ifelse(is.na(new_cases), 0, new_cases)
  )

# -----------------------------
# 5. Plot the time series
# -----------------------------
ggplot(daily_cases_full, aes(x = Meldedatum, y = new_cases)) +
  geom_line() +
  labs(
    title = "Daily New Reported COVID-19 Cases",
    x = "Meldedatum",
    y = "Number of New Cases"
  ) +
  theme_minimal()


# -----------------------------
# 6. Plot ACF and PACF
# -----------------------------
daily_cases_full <- daily_cases_full[order(daily_cases_full$Meldedatum), ] # Sort dates before plotting ACF
cases_ts <- daily_cases_full$new_cases

acf(cases_ts, main = "ACF of Daily New Cases")
pacf(cases_ts, main = "PACF of Daily New Cases")


# 1. Plot ACF
plot(acf_res, xaxt = "n", main = "ACF of Daily New Cases (7-Day Lags)")
# Manually add X-axis: 0 to 35, labeled every 7 units
axis(side = 1, at = seq(0, 35, by = 7))

# 2. Plot PACF
plot(pacf_res, xaxt = "n", main = "PACF of Daily New Cases (7-Day Lags)")
axis(side = 1, at = seq(1, 35, by = 7)) # PACF usually starts from Lag 1
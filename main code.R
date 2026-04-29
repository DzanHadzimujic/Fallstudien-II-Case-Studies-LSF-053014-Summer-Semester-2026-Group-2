library(dplyr)
library(readr)
library(kableExtra)
library(tidyr)

###---1 Data Reading and Cleaning---###

data <- read_csv("data/Aktuell_Deutschland_SarsCov2_Infektionen.csv")

View(data)
View(data2)

invalid_case <- sum(data$NeuerFall == -9, na.rm = TRUE)
invalid_death <- sum(data$NeuerTodesfall == -9, na.rm = TRUE)
invalid_recovery <- sum(data$NeuGenesen == -9, na.rm = TRUE)

invalid_case
invalid_death
invalid_recovery

#table for the variables, counts instances
variable_count_table <- data %>%
  select(NeuerFall, NeuerTodesfall, NeuGenesen) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Value) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Value, values_from = Count, values_fill = 0) %>%
  select(Variable, `-9`, `-1`, `0`, `1`)  # reorder columns

#save and export
variable_count_table %>%
  kable(format = "latex",
        booktabs = TRUE,
        caption = "Distribution of Reporting Status Variables",
        label = "status",
        col.names = c("Variable", "-9", "-1", "0", "1")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  save_kable("variable_count_table.tex")

#sanity checks, preliminary
data <- data %>%
  mutate(
    NeuerFall      = as.numeric(NeuerFall),
    NeuerTodesfall = as.numeric(NeuerTodesfall),
    NeuGenesen     = as.numeric(NeuGenesen)

data %>%
  filter(NeuerTodesfall == -9) %>%
  nrow()        

#count the amount of death cases where NeuerTodesfall was set to -9
count_invalid_death <- sum(data$AnzahlTodesfall[data$NeuerTodesfall == -9])
count_invalid_death

sum_melde_is_ref <- sum(data$Meldedatum == data$Refdatum, na.rm = TRUE)
sum_melde_is_ref


###---2 Autocorrelation Study---###

daily_cases <- data %>%
  group_by(Meldedatum) %>%
  summarise(Faelle = sum(AnzahlFall, na.rm = TRUE))

daily_cases_ref <- data %>%
  group_by(Refdatum) %>%
  summarise(Faelle = sum(AnzahlFall, na.rm = TRUE))

daily_cases$Meldedatum <- as.Date(daily_cases$Meldedatum)
daily_cases_ref$Refdatum <- as.Date(daily_cases_ref$Refdatum)
cases_ts <- ts(daily_cases$Faelle)
cases_ts_ref <- ts(daily_cases_ref$Faelle)

acf_test <- acf(cases_ts, main = "Daily Cases ACF Analysis using Meldedatum")
pacf_test <- pacf(cases_ts, main = "Daily Cases PACF Analysis using Meldedatum")

acf_test_ref <- acf(cases_ts_ref, main = "Daily Cases ACF Analysis using Refdatum")
pacf_test_ref <- pacf(cases_ts_ref, main = "Daily Cases PACF Analysis using Refdatum")

#saving


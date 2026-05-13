### ===========================================================================
### Differencing Playground: find a transformation that avoids very high AR(p)
### ===========================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(forecast)
})

setwd("C:/Users/alvar/Documents/CaseStudies/First_Assignment/")

# -----------------------------
# 1) Data load and cleaning
# -----------------------------
analysis_start <- as.Date("2020-04-01")
analysis_end <- as.Date("2026-03-31")

source_csv <- "Aktuell_Deutschland_SarsCov2_Infektionen.csv"
if (!file.exists(source_csv)) {
  stop("File not found: Aktuell_Deutschland_SarsCov2_Infektionen.csv")
}

covid_raw <- read_csv(
  source_csv,
  show_col_types = FALSE,
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

daily_cases <- covid_clean %>%
  group_by(Meldedatum) %>%
  summarise(value = sum(AnzahlFall), .groups = "drop")

daily_deaths <- covid_clean %>%
  filter(NeuerTodesfall %in% c(0L, 1L)) %>%
  group_by(Meldedatum) %>%
  summarise(value = sum(AnzahlTodesfall), .groups = "drop")

all_dates <- data.frame(
  Meldedatum = seq(
    from = min(daily_cases$Meldedatum),
    to = max(daily_cases$Meldedatum),
    by = "day"
  )
)

daily_cases <- all_dates %>%
  left_join(daily_cases, by = "Meldedatum") %>%
  mutate(value = replace(value, is.na(value), 0L))

daily_deaths <- all_dates %>%
  left_join(daily_deaths, by = "Meldedatum") %>%
  mutate(value = replace(value, is.na(value), 0L))

# -----------------------------
# 2) Helpers
# -----------------------------
weekday_adjust <- function(x, dates) {
  # Remove deterministic weekday mean effects on log scale.
  wd <- factor(weekdays(dates),
               levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  fit <- lm(x ~ wd)
  residuals(fit)
}

apply_scheme <- function(x_log, scheme) {
  y <- x_log

  if (isTRUE(scheme$weekday_adj)) {
    y <- weekday_adjust(y, scheme$dates)
  }

  if (!is.null(scheme$D7) && scheme$D7 > 0) {
    y <- diff(y, lag = 7, differences = scheme$D7)
  }

  if (!is.null(scheme$D365) && scheme$D365 > 0) {
    y <- diff(y, lag = 365, differences = scheme$D365)
  }

  if (!is.null(scheme$d) && scheme$d > 0) {
    y <- diff(y, differences = scheme$d)
  }

  as.numeric(y)
}

safe_lb <- function(x, lag, fitdf = 0) {
  # Keep lag valid relative to sample size and fitted degrees of freedom.
  n <- length(x)
  lag_use <- min(lag, max(5, floor(n / 5)))
  lag_use <- max(lag_use, fitdf + 2)
  lag_use <- min(lag_use, n - 1)
  if (lag_use <= fitdf + 1 || lag_use < 3) return(NA_real_)

  out <- tryCatch(Box.test(x, lag = lag_use, type = "Ljung-Box", fitdf = fitdf)$p.value,
                  error = function(e) NA_real_)
  out
}

evaluate_scheme <- function(series_name, x_log, schemes, max_p = 12) {
  rows <- vector("list", length(schemes))

  for (i in seq_along(schemes)) {
    sc <- schemes[[i]]
    y <- apply_scheme(x_log, sc)

    if (length(y) < 500 || any(!is.finite(y))) {
      rows[[i]] <- data.frame(
        series = series_name,
        scheme = sc$name,
        n = length(y),
        best_p = NA_integer_,
        best_bic = NA_real_,
        raw_lb_p = NA_real_,
        raw_max_abs_acf_1_40 = NA_real_,
        res_lb_p = NA_real_,
        res_max_abs_acf_1_20 = NA_real_,
        note = "invalid or too short"
      )
      next
    }

    a_raw <- acf(y, plot = FALSE, lag.max = 40)$acf[-1]
    raw_lb_p <- safe_lb(y, lag = 40, fitdf = 0)

    fit_list <- vector("list", max_p + 1)
    bic_vec <- rep(NA_real_, max_p + 1)

    for (p in 0:max_p) {
      fit <- tryCatch(
        Arima(y, order = c(p, 0, 0), include.mean = TRUE, method = "ML"),
        error = function(e) NULL
      )
      fit_list[[p + 1]] <- fit
      if (!is.null(fit)) bic_vec[p + 1] <- BIC(fit)
    }

    if (all(is.na(bic_vec))) {
      rows[[i]] <- data.frame(
        series = series_name,
        scheme = sc$name,
        n = length(y),
        best_p = NA_integer_,
        best_bic = NA_real_,
        raw_lb_p = raw_lb_p,
        raw_max_abs_acf_1_40 = max(abs(a_raw), na.rm = TRUE),
        res_lb_p = NA_real_,
        res_max_abs_acf_1_20 = NA_real_,
        note = "all AR fits failed"
      )
      next
    }

    best_idx <- which.min(bic_vec)
    best_p <- best_idx - 1
    best_fit <- fit_list[[best_idx]]
    e <- residuals(best_fit)

    a_res <- acf(e, plot = FALSE, lag.max = 20)$acf[-1]
    res_lb_p <- safe_lb(e, lag = 40, fitdf = best_p)

    rows[[i]] <- data.frame(
      series = series_name,
      scheme = sc$name,
      n = length(y),
      best_p = best_p,
      best_bic = bic_vec[best_idx],
      raw_lb_p = raw_lb_p,
      raw_max_abs_acf_1_40 = max(abs(a_raw), na.rm = TRUE),
      res_lb_p = res_lb_p,
      res_max_abs_acf_1_20 = max(abs(a_res), na.rm = TRUE),
      note = "ok"
    )
  }

  out <- bind_rows(rows)

  # Ranking preference:
  # 1) Residual LB p >= 0.05 (prefer whitened residuals)
  # 2) Lower AR order
  # 3) Lower residual max ACF
  # 4) Lower BIC
  out <- out %>%
    mutate(
      pass_lb = ifelse(!is.na(res_lb_p) & res_lb_p >= 0.05, 1L, 0L),
      best_p_rank = ifelse(is.na(best_p), 999L, best_p),
      res_acf_rank = ifelse(is.na(res_max_abs_acf_1_20), 999, res_max_abs_acf_1_20),
      bic_rank = ifelse(is.na(best_bic), Inf, best_bic)
    ) %>%
    arrange(desc(pass_lb), best_p_rank, res_acf_rank, bic_rank) %>%
    select(-best_p_rank, -res_acf_rank, -bic_rank)

  out
}

# -----------------------------
# 3) Candidate transformations
# -----------------------------
make_schemes <- function(dates) {
  list(
    list(name = "log + d1", d = 1, D7 = 0, D365 = 0, weekday_adj = FALSE, dates = dates),
    list(name = "log + D7", d = 0, D7 = 1, D365 = 0, weekday_adj = FALSE, dates = dates),
    list(name = "log + d1 + D7", d = 1, D7 = 1, D365 = 0, weekday_adj = FALSE, dates = dates),
    list(name = "log + d1 + D365", d = 1, D7 = 0, D365 = 1, weekday_adj = FALSE, dates = dates),
    list(name = "log + d1 + D7 + D365", d = 1, D7 = 1, D365 = 1, weekday_adj = FALSE, dates = dates),
    list(name = "weekday-adjusted log + d1", d = 1, D7 = 0, D365 = 0, weekday_adj = TRUE, dates = dates),
    list(name = "weekday-adjusted log + d1 + D365", d = 1, D7 = 0, D365 = 1, weekday_adj = TRUE, dates = dates),
    list(name = "weekday-adjusted log + d1 + D7", d = 1, D7 = 1, D365 = 0, weekday_adj = TRUE, dates = dates)
  )
}

# -----------------------------
# 4) Run search for cases/deaths
# -----------------------------
log_cases <- log1p(daily_cases$value)
log_deaths <- log1p(daily_deaths$value)

schemes_cases <- make_schemes(daily_cases$Meldedatum)
schemes_deaths <- make_schemes(daily_deaths$Meldedatum)

res_cases <- evaluate_scheme("cases", log_cases, schemes_cases, max_p = 12)
res_deaths <- evaluate_scheme("deaths", log_deaths, schemes_deaths, max_p = 12)

cat("\n================ DIFFERENCING PLAYGROUND RESULTS ================\n")
cat("\nTop candidates for CASES:\n")
print(head(res_cases, 8), row.names = FALSE)

cat("\nTop candidates for DEATHS:\n")
print(head(res_deaths, 8), row.names = FALSE)

# -----------------------------
# 5) Persist outputs for report
# -----------------------------
out_dir <- file.path(getwd(), "plots")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(res_cases, file.path(out_dir, "playground_cases_ranking.csv"), row.names = FALSE)
write.csv(res_deaths, file.path(out_dir, "playground_deaths_ranking.csv"), row.names = FALSE)

cat("\nSaved:\n")
cat(" - ", file.path(out_dir, "playground_cases_ranking.csv"), "\n", sep = "")
cat(" - ", file.path(out_dir, "playground_deaths_ranking.csv"), "\n", sep = "")

# -----------------------------
# 6) Optional quick diagnostic plots for top scheme
# -----------------------------
plot_top_scheme <- function(x_log, schemes, res_tbl, series_label, png_name) {
  best_name <- res_tbl$scheme[1]
  sc <- schemes[[which(vapply(schemes, function(z) z$name, character(1)) == best_name)[1]]]
  y <- apply_scheme(x_log, sc)

  png(filename = file.path(out_dir, png_name), width = 1500, height = 900, res = 140)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  plot(y, type = "l", col = "steelblue", main = paste(series_label, "- Top transformed series:"),
       xlab = "Index", ylab = "Value")
  mtext(best_name, side = 3, line = 0.2, cex = 0.8)

  Acf(y, lag.max = 60, main = paste("ACF -", series_label))
  Pacf(y, lag.max = 60, main = paste("PACF -", series_label))

  fit <- Arima(y, order = c(res_tbl$best_p[1], 0, 0), include.mean = TRUE, method = "ML")
  Acf(residuals(fit), lag.max = 60, main = paste("Residual ACF - AR(", res_tbl$best_p[1], ")", sep = ""))

  par(mfrow = c(1, 1))
  dev.off()
}

plot_top_scheme(log_cases, schemes_cases, res_cases, "Cases", "playground_cases_top_scheme.png")
plot_top_scheme(log_deaths, schemes_deaths, res_deaths, "Deaths", "playground_deaths_top_scheme.png")

cat(" - ", file.path(out_dir, "playground_cases_top_scheme.png"), "\n", sep = "")
cat(" - ", file.path(out_dir, "playground_deaths_top_scheme.png"), "\n", sep = "")

cat("\nDone. Use the top ranked scheme for AR identification if it keeps best_p modest and residual LB p-value acceptable.\n")


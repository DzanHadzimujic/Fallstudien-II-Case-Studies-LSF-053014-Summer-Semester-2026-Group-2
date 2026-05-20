
import os, warnings, requests
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from scipy import signal
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.stattools import adfuller, kpss as kpss_test

warnings.filterwarnings("ignore")

# ==============================================================================
# 1. download data
#    R: download.file(url, destfile, mode="wb")
# ==============================================================================
URL = (
    "https://media.githubusercontent.com/media/robert-koch-institut/"
    "SARS-CoV-2-Infektionen_in_Deutschland/refs/heads/main/"
    "Aktuell_Deutschland_SarsCov2_Infektionen.csv"
)
FILE_NAME = "Aktuell_Deutschland_SarsCov2_Infektionen.csv"

if not os.path.exists(FILE_NAME):
    print("Downloading data (~400 MB)...")
    resp = requests.get(URL, stream=True, timeout=600)
    resp.raise_for_status()
    with open(FILE_NAME, "wb") as f:
        for chunk in resp.iter_content(chunk_size=65536):
            f.write(chunk)
    print("Download complete.")
else:
    print(f"File already exists: {FILE_NAME}")

# ==============================================================================
# 2. read CSV with types
#    R: read_csv(file_path, col_types = cols(...))
# ==============================================================================
print("Reading CSV...")
df = pd.read_csv(
    FILE_NAME,
    dtype={
        "IdLandkreis": "Int32", "Altersgruppe": str, "Geschlecht": str,
        "IstErkrankungsbeginn": "Int32", "NeuerFall": "Int32",
        "NeuerTodesfall": "Int32", "NeuGenesen": "Int32",
        "AnzahlFall": "Int32", "AnzahlTodesfall": "Int32", "AnzahlGenesen": "Int32",
    },
    parse_dates=["Meldedatum", "Refdatum"],
)

ANALYSIS_START = pd.Timestamp("2020-04-01")
ANALYSIS_END = pd.Timestamp("2026-03-31")
df = df[(df["Meldedatum"] >= ANALYSIS_START) & (df["Meldedatum"] <= ANALYSIS_END)].copy()

print(f"Raw data: {df.shape[0]:,} rows x {df.shape[1]} cols")
print(f"Date range: {df['Meldedatum'].min().date()} to {df['Meldedatum'].max().date()}")
print("\nNeuerFall:\n", df["NeuerFall"].value_counts().sort_index().to_string())
print("\nNeuerTodesfall:\n", df["NeuerTodesfall"].value_counts().sort_index().to_string())
print("\nMissing values:\n", df.isna().sum().to_string())

# ==============================================================================
# 3. cleaning
#    R: filter(NeuerFall %in% c(0L, 1L))
# ==============================================================================
covid_clean = df[df["NeuerFall"].isin([0, 1])].copy()
print(f"\nAfter filter: {len(covid_clean):,} rows")
print("Negative AnzahlFall:", (covid_clean["AnzahlFall"] < 0).any())
print("Negative AnzahlTodesfall:", (covid_clean["AnzahlTodesfall"] < 0).any())

# ==============================================================================
# 4. group by date to get daily series
#    R: group_by(Meldedatum) %>% summarise(cases = sum(AnzahlFall))
# ==============================================================================
daily_cases = (covid_clean.groupby("Meldedatum")["AnzahlFall"].sum().rename("cases"))
daily_deaths = (covid_clean[covid_clean["NeuerTodesfall"].isin([0, 1])]
                .groupby("Meldedatum")["AnzahlTodesfall"].sum().rename("deaths"))
daily_recovered = (covid_clean[covid_clean["NeuGenesen"].isin([0, 1])]
                   .groupby("Meldedatum")["AnzahlGenesen"].sum().rename("recovered"))

# R: left_join + replace(is.na, 0)
full_idx = pd.date_range(daily_cases.index.min(), daily_cases.index.max(), freq="D")
daily_cases = daily_cases.reindex(full_idx, fill_value=0)
daily_deaths = daily_deaths.reindex(full_idx, fill_value=0)
daily_recovered = daily_recovered.reindex(full_idx, fill_value=0)

print(f"\nDaily series: {len(daily_cases)} days")
print(f"Total cases:     {daily_cases.sum():,}")
print(f"Total deaths:    {daily_deaths.sum():,}")
print(f"Total recovered: {daily_recovered.sum():,}")

# ==============================================================================
# 5. log1p transformation
#    R: log1p(ts_cases)
# ==============================================================================
log_cases = np.log1p(daily_cases.values.astype(float))
log_deaths = np.log1p(daily_deaths.values.astype(float))
log_recovered = np.log1p(daily_recovered.values.astype(float))


# ==============================================================================
# 5a. periodogram
#     R: spec.pgram(log1p(series), demean=TRUE, detrend=TRUE)
# ==============================================================================
def detect_seasonal_period(series, min_period=2, max_period=30):
    x = series - series.mean()
    freqs, power = signal.periodogram(x)
    with np.errstate(divide="ignore"):
        periods = np.where(freqs > 0, 1.0 / freqs, np.inf)
    mask = (periods >= min_period) & (periods <= max_period)
    if not mask.any():
        return 7
    return max(2, int(round(periods[mask][np.argmax(power[mask])])))


season_cases = detect_seasonal_period(log_cases)
season_deaths = detect_seasonal_period(log_deaths)
season_recovered = detect_seasonal_period(log_recovered)

print(f"\nSeasonal period - cases:{season_cases}  deaths:{season_deaths}  recovered:{season_recovered}")


# difference with lag=season_cases and differences=1
# R: diff(diff(log1p(ts_cases), lag=season_cases), differences=1)
def double_diff(x, lag):
    s = pd.Series(x)
    return s.diff(lag).diff(1).dropna().values


ts_cases_id = double_diff(log_cases, season_cases)
ts_deaths_id = double_diff(log_deaths, season_deaths)
ts_recovered_id = double_diff(log_recovered, season_recovered)


# ==============================================================================
# 5b. stationary test
#     R: adf.test() + kpss.test()
# ==============================================================================
def run_stationarity_tests(x, name):
    _, adf_p, *_ = adfuller(x, autolag="AIC")
    _, kpss_p, *_ = kpss_test(x, regression="c", nlags="auto")
    a = "stationary    " if adf_p < 0.05 else "NON-stationary"
    k = "stationary    " if kpss_p > 0.05 else "NON-stationary"
    print(f"{name:<12} | ADF p={adf_p:.4f} ({a}) | KPSS p={kpss_p:.4f} ({k})")


print("\nStationarity tests:")
print("-" * 72)
run_stationarity_tests(ts_cases_id, "Cases")
run_stationarity_tests(ts_deaths_id, "Deaths")
run_stationarity_tests(ts_recovered_id, "Recovered")

PLOT_DIR = "plots"
os.makedirs(PLOT_DIR, exist_ok=True)

# ==============================================================================
# 6. time series plots
#    R: plot(date, value, type="l", col=...)
# ==============================================================================
fig, axes = plt.subplots(3, 1, figsize=(14, 10))
for ax, s, title, c in zip(
        axes,
        [daily_cases, daily_deaths, daily_recovered],
        ["Daily Covid-19 Cases", "Daily Covid-19 Deaths", "Daily Covid-19 Recovered"],
        ["steelblue", "firebrick", "darkgreen"]
):
    ax.plot(s.index, s.values, color=c, lw=0.7)
    ax.set_title(title);
    ax.set_xlabel("Date");
    ax.set_ylabel("Count")
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    plt.setp(ax.xaxis.get_majorticklabels(), rotation=30)
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "01_time_series.png"), dpi=150);
plt.show()

# ==============================================================================
# 7. periodogram
#    R: spec.pgram(log1p(ts_cases), ...)
# ==============================================================================
fig, axes = plt.subplots(3, 1, figsize=(14, 10))
for ax, x, label, season in zip(
        axes,
        [log_cases, log_deaths, log_recovered],
        ["Cases", "Deaths", "Recovered"],
        [season_cases, season_deaths, season_recovered]
):
    freqs, power = signal.periodogram(x - x.mean())
    with np.errstate(divide="ignore"):
        periods = np.where(freqs > 0, 1.0 / freqs, np.inf)
    mask = (periods > 1.5) & (periods < 400)
    ax.semilogy(periods[mask], power[mask], color="steelblue", lw=0.7)
    ax.axvline(season, color="red", ls="--", label=f"period={season}")
    ax.axvline(7, color="orange", ls=":", label="7-day")
    ax.set_title(f"Periodogram – {label}");
    ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "02_periodogram.png"), dpi=150);
plt.show()

# ==============================================================================
# 8. ACF / PACF
#    R: Acf(ts_cases_id, lag.max=60) / Pacf(...)
# ==============================================================================
fig, axes = plt.subplots(3, 2, figsize=(16, 12))
for row, (ts_id, label) in enumerate(zip(
        [ts_cases_id, ts_deaths_id, ts_recovered_id],
        ["Cases", "Deaths", "Recovered"]
)):
    plot_acf(ts_id, ax=axes[row, 0], lags=60, title=f"ACF – {label} (transformed)")
    plot_pacf(ts_id, ax=axes[row, 1], lags=60, method="ywm",
              title=f"PACF – {label} (transformed)")
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "03_acf_pacf.png"), dpi=150);
plt.show()

print(f"\nSaved plots to: {PLOT_DIR}/")


# ==============================================================================
# 9. Task (b): AR(p) order selection（BIC）
#    R: select_ar_order(ts_cases_id, max_p=3, criterion="BIC")
# ==============================================================================
def select_ar_order(y, max_p=3):
    results = [];
    best_bic = np.inf;
    best_p = 0;
    prev_bic = np.inf
    for p in range(0, max_p + 1):
        try:
            res = ARIMA(y, order=(p, 0, 0), trend="c").fit()
            bic = res.bic
            results.append({"p": p, "BIC": round(bic, 4)})
            if bic < best_bic:
                best_bic = bic;
                best_p = p
            if bic > prev_bic:
                break
            prev_bic = bic
        except Exception:
            results.append({"p": p, "BIC": float("nan")})
    return best_p, pd.DataFrame(results)


p_cases, bic_df_c = select_ar_order(ts_cases_id, max_p=3)
p_deaths, bic_df_d = select_ar_order(ts_deaths_id, max_p=3)
p_recovered, bic_df_r = select_ar_order(ts_recovered_id, max_p=3)

print(f"\nBest AR order — Cases:{p_cases}  Deaths:{p_deaths}  Recovered:{p_recovered}")
print("\nBIC table – Cases:\n", bic_df_c.to_string(index=False))
print("\nBIC table – Deaths:\n", bic_df_d.to_string(index=False))
print("\nBIC table – Recovered:\n", bic_df_r.to_string(index=False))

fig, axes = plt.subplots(3, 1, figsize=(8, 9))
for ax, bdf, label, bp, color in zip(
        axes,
        [bic_df_c, bic_df_d, bic_df_r],
        ["Cases", "Deaths", "Recovered"],
        [p_cases, p_deaths, p_recovered],
        ["steelblue", "firebrick", "darkgreen"]
):
    ax.plot(bdf["p"], bdf["BIC"], "o-", color=color)
    ax.axvline(bp, color="red", ls="--", label=f"best p={bp}")
    ax.set_title(f"BIC – {label}");
    ax.set_xlabel("p");
    ax.set_ylabel("BIC");
    ax.legend()
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "05_ar_bic.png"), dpi=150);
plt.show()


# ==============================================================================
# 10. Task (c): AR forecast（80/20）
#     R: forecast_ar(log_cases, ar_cases$best_p, split=0.80)
# ==============================================================================
def forecast_ar(y_log, best_p, split=0.80):
    n = len(y_log);
    n_train = int(n * split);
    n_test = n - n_train
    y_train = y_log[:n_train];
    y_test = y_log[n_train:]
    res = ARIMA(y_train, order=(best_p, 1, 0), trend="n").fit()
    yhat = res.forecast(steps=n_test)
    msfe = float(np.sqrt(np.mean((y_test - yhat) ** 2)))
    return {"yhat": yhat, "yact": y_test, "msfe": msfe,
            "n_train": n_train, "n_test": n_test}


fc_cases = forecast_ar(log_cases, p_cases)
fc_deaths = forecast_ar(log_deaths, p_deaths)
fc_recovered = forecast_ar(log_recovered, p_recovered)

print("\n--- Task (c): AR MSFE (80/20, log1p) ---")
print(f"Cases     AR({p_cases}) | MSFE = {fc_cases['msfe']:.6f}")
print(f"Deaths    AR({p_deaths}) | MSFE = {fc_deaths['msfe']:.6f}")
print(f"Recovered AR({p_recovered}) | MSFE = {fc_recovered['msfe']:.6f}")


def plot_fc_panel(ax, fc, dates, title, ca, cf):
    td = dates[fc["n_train"]: fc["n_train"] + fc["n_test"]]
    ax.plot(td, fc["yact"], color=ca, lw=0.8, label="Actual (test)")
    ax.plot(td, fc["yhat"], color=cf, lw=1.5, ls="--", label="Forecast")
    ax.set_title(title);
    ax.set_xlabel("Date");
    ax.set_ylabel("log1p");
    ax.legend(fontsize=9)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y-%m"))
    plt.setp(ax.xaxis.get_majorticklabels(), rotation=30)


fig, axes = plt.subplots(3, 1, figsize=(14, 11))
plot_fc_panel(axes[0], fc_cases, full_idx, f"Cases AR({p_cases}) RMSFE={fc_cases['msfe']:.4f}", "steelblue", "orange")
plot_fc_panel(axes[1], fc_deaths, full_idx, f"Deaths AR({p_deaths}) RMSFE={fc_deaths['msfe']:.4f}", "firebrick",
              "purple")
plot_fc_panel(axes[2], fc_recovered, full_idx, f"Recovered AR({p_recovered}) RMSFE={fc_recovered['msfe']:.4f}",
              "darkgreen", "darkorange")
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "06_ar_forecast.png"), dpi=150);
plt.show()

# ==============================================================================
# 11. Task (d): ARX
# ==============================================================================

# 11.1 age group shares
covid_age = covid_clean.copy()
covid_age["is_60plus"] = covid_age["Altersgruppe"].str.match(r"^A[6-9]")
covid_age["is_80plus"] = covid_age["Altersgruppe"].str.match(r"^A8")


def age_share(grp, val_col, flag_col):
    total = grp[val_col].sum()
    pos = grp.loc[grp[flag_col], val_col].sum()
    return pos / total if total > 0 else 0.0


age_c = covid_age.groupby("Meldedatum").apply(
    lambda g: pd.Series({
        "share_cases_60plus": age_share(g, "AnzahlFall", "is_60plus")
    })
)
age_d = (covid_age[covid_age["NeuerTodesfall"].isin([0, 1])]
.groupby("Meldedatum").apply(
    lambda g: pd.Series({
        "share_deaths_80plus": age_share(g, "AnzahlTodesfall", "is_80plus")
    })
))

# 11.2 temperature data（Open-Meteo））
temp_url = (
    "https://archive-api.open-meteo.com/v1/archive?"
    "latitude=51.1657&longitude=10.4515"
    f"&start_date={ANALYSIS_START.date()}&end_date={ANALYSIS_END.date()}"
    "&daily=temperature_2m_mean&timezone=Europe%2FBerlin"
)
tr = requests.get(temp_url, timeout=60).json()
temp_s = pd.Series(
    tr["daily"]["temperature_2m_mean"],
    index=pd.to_datetime(tr["daily"]["time"]),
    name="temp_c"
).interpolate().ffill().bfill()

# 11.3 merge
pred = pd.DataFrame(index=full_idx)
pred = pred.join(temp_s, how="left")
pred = pred.join(age_c["share_cases_60plus"], how="left")
pred = pred.join(age_d["share_deaths_80plus"], how="left")
pred["share_cases_60plus"] = pred["share_cases_60plus"].fillna(0)
pred["share_deaths_80plus"] = pred["share_deaths_80plus"].fillna(0)
pred["temp_c"] = pred["temp_c"].interpolate().ffill().bfill()
pred["is_weekend"] = (pred.index.dayofweek >= 5).astype(int)
pred["time_idx"] = np.arange(1, len(pred) + 1, dtype=float)

print(f"\nTemp mean: {pred['temp_c'].mean():.2f}°C  "
      f"share_60+: {pred['share_cases_60plus'].mean():.4f}  "
      f"share_80+: {pred['share_deaths_80plus'].mean():.4f}")

xreg_c = pred[["temp_c", "is_weekend", "time_idx", "share_cases_60plus"]].values
xreg_d = pred[["temp_c", "is_weekend", "time_idx", "share_deaths_80plus"]].values
xreg_r = pred[["temp_c", "is_weekend", "time_idx", "share_cases_60plus"]].values


# 11.4 ARX forecasting function
def forecast_arx(y_log, xreg, best_p, split=0.80):
    n = len(y_log);
    n_train = int(n * split);
    n_test = n - n_train
    res = ARIMA(y_log[:n_train], order=(best_p, 1, 0),
                exog=xreg[:n_train], trend="n").fit()
    yhat = res.forecast(steps=n_test, exog=xreg[n_train:])
    yact = y_log[n_train:]
    return {"yhat": yhat, "yact": yact,
            "msfe": float(np.sqrt(np.mean((yact - yhat) ** 2))),
            "n_train": n_train, "n_test": n_test}


arx_cases = forecast_arx(log_cases, xreg_c, p_cases)
arx_deaths = forecast_arx(log_deaths, xreg_d, p_deaths)
arx_recovered = forecast_arx(log_recovered, xreg_r, p_recovered)

print("\n--- Task (d): AR vs ARX MSFE (log1p) ---")
print(f"Cases:     AR={fc_cases['msfe']:.6f} | ARX={arx_cases['msfe']:.6f}")
print(f"Deaths:    AR={fc_deaths['msfe']:.6f} | ARX={arx_deaths['msfe']:.6f}")
print(f"Recovered: AR={fc_recovered['msfe']:.6f} | ARX={arx_recovered['msfe']:.6f}")

fig, axes = plt.subplots(3, 1, figsize=(14, 11))
plot_fc_panel(axes[0], arx_cases, full_idx, f"Cases ARX({p_cases}) RMSFE={arx_cases['msfe']:.4f}", "steelblue", "orange")
plot_fc_panel(axes[1], arx_deaths, full_idx, f"Deaths ARX({p_deaths}) RMSFE={arx_deaths['msfe']:.4f}", "firebrick",
              "purple")
plot_fc_panel(axes[2], arx_recovered, full_idx, f"Recovered ARX({p_recovered}) RMSFE={arx_recovered['msfe']:.4f}",
              "darkgreen", "darkorange")
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "07_arx_forecast.png"), dpi=150);
plt.show()


# ==============================================================================
# 12. Task (e): seasonal AR  Φ(B^365)φ(B)y_t = ε_t
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
import os




def forecast_ols_sarima(y_log_series, p_val, split=0.80, season=365):
    y_log = np.array(y_log_series)
    n = len(y_log)
    n_train = int(n * split)
    n_test = n - n_train

    y_log_train = y_log[:n_train]
    y_log_test = y_log[n_train:]


    #  z_t = y_t - y_{t-1} - y_{t-365} + y_{t-366}
    # ---------------------------------------------------------
    y_stat_train = np.zeros(n_train)
    for t in range(season + 1, n_train):
        y_stat_train[t] = y_log_train[t] - y_log_train[t - 1] - y_log_train[t - season] + y_log_train[t - season - 1]

    # ---------------------------------------------------------
    # 2. OLS
    # ---------------------------------------------------------
    max_lag = season + p_val
    Y_target = []
    X_features = []


    for t in range(season + 1 + max_lag, n_train):
        Y_target.append(y_stat_train[t])

        row = []

        for i in range(1, p_val + 1):
            row.append(y_stat_train[t - i])
        # lag 365
        row.append(y_stat_train[t - season])
        # cross lag 366 to 365+p_val)
        for i in range(1, p_val + 1):
            row.append(y_stat_train[t - season - i])

        X_features.append(row)

    Y_target = np.array(Y_target)
    X_features = np.array(X_features)

    # ---------------------------------------------------------
    # 3. OLS model
    # ---------------------------------------------------------
    # print(f"Training OLS matrix with shape: X={X_features.shape}, Y={Y_target.shape}")
    ols_model = sm.OLS(Y_target, X_features)
    results = ols_model.fit()
    coefs = results.params

    # ---------------------------------------------------------
    # 4. Dynamic Forecasting
    # ---------------------------------------------------------
    current_y_stat = list(y_stat_train)
    current_y_log = list(y_log_train)
    yhat = []

    for step in range(n_test):
        t_stat = len(current_y_stat)


        row = []
        for i in range(1, p_val + 1):
            row.append(current_y_stat[t_stat - i])
        row.append(current_y_stat[t_stat - season])
        for i in range(1, p_val + 1):
            row.append(current_y_stat[t_stat - season - i])

        # z_hat
        next_stat = np.dot(coefs, row)
        current_y_stat.append(next_stat)


        t_log = len(current_y_log)
        next_log = next_stat + current_y_log[t_log - 1] + current_y_log[t_log - season] - current_y_log[
            t_log - season - 1]

        current_y_log.append(next_log)
        yhat.append(next_log)

    # ---------------------------------------------------------
    # 5. MSFE Calculation
    # ---------------------------------------------------------
    yhat = np.array(yhat)
    yact = y_log_test
    msfe = float(np.sqrt(np.mean((yact - yhat) ** 2)))

    return {"yhat": yhat, "yact": yact, "msfe": msfe,
            "n_train": n_train, "n_test": n_test}


sar_cases_ols = forecast_ols_sarima(log_cases, p_cases)
sar_deaths_ols = forecast_ols_sarima(log_deaths, p_deaths)
sar_recovered_ols = forecast_ols_sarima(log_recovered, p_recovered)

print("\n--- AR vs ARX vs OLS SAR (log1p, 80/20) ---")
print(f"Cases:     AR={fc_cases['msfe']:.6f} | ARX={arx_cases['msfe']:.6f} | OLS_SAR={sar_cases_ols['msfe']:.6f}")
print(f"Deaths:    AR={fc_deaths['msfe']:.6f} | ARX={arx_deaths['msfe']:.6f} | OLS_SAR={sar_deaths_ols['msfe']:.6f}")
print(
    f"Recovered: AR={fc_recovered['msfe']:.6f} | ARX={arx_recovered['msfe']:.6f} | OLS_SAR={sar_recovered_ols['msfe']:.6f}")

# --- plot ---
fig, axes = plt.subplots(3, 1, figsize=(14, 11))
plot_fc_panel(axes[0], sar_cases_ols, full_idx, f"Cases OLS_SAR({p_cases})_365 MSFE={sar_cases_ols['msfe']:.4f}",
              "steelblue", "orange")
plot_fc_panel(axes[1], sar_deaths_ols, full_idx, f"Deaths OLS_SAR({p_deaths})_365 MSFE={sar_deaths_ols['msfe']:.4f}",
              "firebrick", "purple")
plot_fc_panel(axes[2], sar_recovered_ols, full_idx,
              f"Recovered OLS_SAR({p_recovered})_365 MSFE={sar_recovered_ols['msfe']:.4f}", "darkgreen", "darkorange")
plt.tight_layout()
plt.savefig(os.path.join(PLOT_DIR, "08_ols_sar_forecast.png"), dpi=150)
plt.show()
url <- "https://media.githubusercontent.com/media/robert-koch-institut/SARS-CoV-2-Infektionen_in_Deutschland/refs/heads/main/Aktuell_Deutschland_SarsCov2_Infektionen.csv"
out_file <- file.path(getwd(), "Aktuell_Deutschland_SarsCov2_Infektionen_online_test.csv")

cat("Working directory:", getwd(), "\n")
cat("Downloading from:\n", url, "\n")

status <- tryCatch(
  download.file(url = url, destfile = out_file, mode = "wb", quiet = FALSE),
  error = function(e) e
)

if (inherits(status, "error")) {
  cat("DOWNLOAD_ERROR:\n", conditionMessage(status), "\n")
  quit(save = "no", status = 1)
}

if (!file.exists(out_file)) {
  cat("DOWNLOAD_FAILED: file was not created.\n")
  quit(save = "no", status = 1)
}

size_bytes <- file.info(out_file)$size
cat("Download status code:", status, "\n")
cat("Saved file:", out_file, "\n")
cat("File size (bytes):", size_bytes, "\n")

# Read only a few rows to confirm parse works
suppressPackageStartupMessages(library(readr))
preview <- read_csv(out_file, n_max = 5)
cat("Rows read in preview:", nrow(preview), "\n")
print(preview)

cat("ONLINE_DOWNLOAD_TEST: SUCCESS\n")

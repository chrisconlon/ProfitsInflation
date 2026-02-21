# Install all R packages required by this replication package.
# Run once before executing any analysis scripts.

packages <- c(
  # Data wrangling
  "tidyverse",
  "data.table",
  "zoo",

  # Database access (WRDS / Compustat)
  "DBI",
  "RPostgres",

  # Parquet I/O
  "arrow",

  # String interpolation
  "glue",

  # Econometrics
  "lfe",

  # Visualization
  "ggplot2",
  "gridExtra",
  "grid",
  "scales",

  # Excel I/O
  "openxlsx",

  # FRED API
  "fredr",

  # Config and path utilities
  "ini",
  "here"
)

install.packages(setdiff(packages, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")

cat("All packages installed.\n")

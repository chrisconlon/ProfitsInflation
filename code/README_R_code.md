# inflation

## Setup

1. Copy `config.ini.example` to `config.ini` and fill in your WRDS, FRED, and BLS credentials.
2. Run `install_packages.R` to install all required R packages.
3. Run `00_download_wrds.R` to download Compustat data from WRDS and save it locally as parquet files in `raw_data/`. This only needs to be run once (or again if you want to refresh the data).
4. Run the remaining scripts (`01`–`10`) in any order. They read from the local parquet files and do not require a WRDS connection.

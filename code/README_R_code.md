# R code

## Setup

1. Copy `config.ini.example` to `config.ini` in the project root and fill in your WRDS, FRED, and BLS credentials.
2. Run `install_packages.R` to install all required R packages.
3. Run `python code/download_data.py` to download all external data (BEA, Census, and Compustat) to `raw_data/`.
4. Run the scripts (`01`–`10`) in any order, except that `05` must be run before `06`. They read from local files and do not require live database connections.

"""Download BEA, Census, and WRDS/Compustat data files to raw_data/."""
import configparser
import io
import urllib.request
import zipfile
from pathlib import Path

RAW_DIR = Path(__file__).resolve().parent.parent / "raw_data"
RAW_DIR.mkdir(exist_ok=True)

DOWNLOADS = {
    "Section1All_xls.xlsx": "https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx",
    "Section6All_xls.xlsx": "https://apps.bea.gov/national/Release/XLS/Survey/Section6All_xls.xlsx",
    "GrossOutput.xlsx": "https://apps.bea.gov/industry/Release/XLS/GDPxInd/GrossOutput.xlsx",
    "2-6 digit_2017_Codes.xlsx": "https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx",
}

# Census QFR: downloaded as a zip, extracted to raw_data/qfr/
QFR_URL = "https://www.census.gov/econ_getzippedfile/?programCode=QFR"
QFR_DIR = RAW_DIR / "qfr"
QFR_FILE = QFR_DIR / "QFR-mf.csv"

# WRDS/Compustat queries
WRDS_QUERIES = {
    "compustat_fundq.parquet": """
        SELECT gvkey, conm, datacqtr, datadate, indfmt, datafmt, consol,
               ibq, oibdpq, saleq, cogsq, fic, curcdq
        FROM comp.fundq
        WHERE date_part('year', datadate) >= 2010
    """,
    "compustat_funda.parquet": """
        SELECT gvkey, conm, datadate, datafmt, indfmt, consol,
               naicsh, ib, oibdp, sale
        FROM comp.funda
        WHERE date_part('year', datadate) >= 2010
    """,
}


def download(url, dest):
    """Download a URL to a local file (with browser User-Agent)."""
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req) as resp, open(dest, "wb") as f:
        f.write(resp.read())


# --- BEA / Census direct downloads -------------------------------------------
for filename, url in DOWNLOADS.items():
    dest = RAW_DIR / filename
    if not dest.exists():
        print(f"Downloading {filename}...")
        download(url, dest)
        print(f"  Saved to {dest}")
    else:
        print(f"{filename} already exists, skipping.")

# QFR zip download
if not QFR_FILE.exists():
    print("Downloading Census QFR data...")
    req = urllib.request.Request(QFR_URL, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req) as resp:
        data = resp.read()
    QFR_DIR.mkdir(exist_ok=True)
    with zipfile.ZipFile(io.BytesIO(data)) as zf:
        zf.extractall(QFR_DIR)
    print(f"  Extracted to {QFR_DIR}")
else:
    print("QFR-mf.csv already exists, skipping.")

# --- WRDS / Compustat --------------------------------------------------------
# Skip if all parquet files already exist
wrds_needed = {f: q for f, q in WRDS_QUERIES.items()
               if not (RAW_DIR / f).exists()}

if wrds_needed:
    # Read WRDS credentials from config.ini
    config_path = RAW_DIR.parent / "config.ini"
    config = configparser.ConfigParser()
    config.read(config_path)

    wrds_user = config.get("wrds", "wrds_user", fallback="")
    wrds_pass = config.get("wrds", "wrds_password", fallback="")

    if not wrds_user or not wrds_pass or "YOUR_" in wrds_user:
        print("WRDS credentials not configured in config.ini — skipping Compustat download.")
        print("  (Fill in [wrds] wrds_user / wrds_password to enable.)")
    else:
        try:
            import wrds
            conn = wrds.Connection(wrds_username=wrds_user, wrds_password=wrds_pass)
            for filename, sql in wrds_needed.items():
                dest = RAW_DIR / filename
                print(f"Downloading {filename} from WRDS...")
                df = conn.raw_sql(sql)
                print(f"  {len(df)} rows")
                df.to_parquet(dest, index=False)
                print(f"  Saved to {dest}")
            conn.close()
        except ImportError:
            print("wrds package not installed — skipping Compustat download.")
            print("  Install with: pip install wrds pyarrow")
        except Exception as e:
            print(f"WRDS download failed: {e}")
else:
    print("Compustat parquet files already exist, skipping WRDS download.")

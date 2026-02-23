"""Download BEA and Census data files to raw_data/."""
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

def download(url, dest):
    """Download a URL to a local file (with browser User-Agent)."""
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req) as resp, open(dest, "wb") as f:
        f.write(resp.read())

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

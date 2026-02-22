"""Download BEA data files to raw_data/."""
import urllib.request
from pathlib import Path

RAW_DIR = Path(__file__).resolve().parent.parent / "raw_data"
RAW_DIR.mkdir(exist_ok=True)

DOWNLOADS = {
    "Section1All_xls.xlsx": "https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx",
    "Section6All_xls.xlsx": "https://apps.bea.gov/national/Release/XLS/Survey/Section6All_xls.xlsx",
    # Future: add gross_output_by_industry URL here
}

for filename, url in DOWNLOADS.items():
    dest = RAW_DIR / filename
    if not dest.exists():
        print(f"Downloading {filename}...")
        urllib.request.urlretrieve(url, dest)
        print(f"  Saved to {dest}")
    else:
        print(f"{filename} already exists, skipping.")

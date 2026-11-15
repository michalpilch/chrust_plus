#!/usr/bin/env python3
"""
Secure Telemetry Ingestion from Domoticz to Google Cloud BigQuery.
Installation: chrust_plus_01 (HappySt8, Rogozino)
Target: heating-iq-2026.heating_telemetry.sensor_readings
"""

import os
import sqlite3
from datetime import datetime, timezone
from typing import List, Dict, Any

from google.cloud import bigquery
from google.oauth2 import service_account

# ============================================================
# Configuration
# ============================================================
INSTALLATION_ID = "chrust_plus_01"
PROJECT_ID = "heating-iq-2026"
DATASET_ID = "heating_telemetry"
TABLE_ID = "sensor_readings"
FULL_TABLE_REF = f"{PROJECT_ID}.{DATASET_ID}.{TABLE_ID}"

DOMOTICZ_DB = "/home/dsl/domoticz/domoticz.db"
KEY_FILE = "/home/dsl/.config/gcp/heating-iq-iot-collector.json"

SENSOR_MAP = {
    1: "temp_piec",
    2: "temp_co",
    3: "bufor_top",
    4: "bufor_mid1",
    5: "bufor_mid2",
    6: "bufor_bottom",
    7: "temp_home",
    8: "temp_ext",
}


def get_bigquery_client() -> bigquery.Client:
    """Create BigQuery client using scoped Service Account credentials."""
    credentials = service_account.Credentials.from_service_account_file(
        KEY_FILE,
        scopes=["https://www.googleapis.com/auth/bigquery"],
    )
    return bigquery.Client(project=PROJECT_ID, credentials=credentials, location="europe-central2")


def get_last_timestamp(client: bigquery.Client) -> datetime | None:
    """Get the latest ingested timestamp for this installation from BigQuery."""
    query = f"""
        SELECT MAX(timestamp) as max_time
        FROM `{FULL_TABLE_REF}`
        WHERE installation_id = @inst_id
    """
    job_config = bigquery.QueryJobConfig(
        query_parameters=[
            bigquery.ScalarQueryParameter("inst_id", "STRING", INSTALLATION_ID)
        ]
    )
    results = client.query(query, job_config=job_config).result()
    for row in results:
        if row.max_time:
            return row.max_time
    return None


def fetch_domoticz_records(since_datetime: datetime | None) -> List[Dict[str, Any]]:
    """Fetch and pivot 5-minute temperature records from local SQLite."""
    con = sqlite3.connect(f"file:{DOMOTICZ_DB}?mode=ro", uri=True)
    
    if since_datetime:
        # SQLite format: YYYY-MM-DD HH:MM:SS
        since_str = since_datetime.strftime("%Y-%m-%d %H:%M:%S")
        query = """
            SELECT Date, DeviceRowID, Temperature
            FROM Temperature
            WHERE DeviceRowID IN (1,2,3,4,5,6,7,8)
              AND Date > ?
            ORDER BY Date ASC
        """
        rows = con.execute(query, (since_str,)).fetchall()
    else:
        # Initial sync: past 7 days
        query = """
            SELECT Date, DeviceRowID, Temperature
            FROM Temperature
            WHERE DeviceRowID IN (1,2,3,4,5,6,7,8)
            ORDER BY Date ASC
        """
        rows = con.execute(query).fetchall()
        
    con.close()
    
    # Pivot rows by timestamp
    grouped: Dict[str, Dict[str, float]] = {}
    for date_str, dev_id, temp in rows:
        if date_str not in grouped:
            grouped[date_str] = {}
        sensor_name = SENSOR_MAP.get(dev_id)
        if sensor_name:
            grouped[date_str][sensor_name] = temp
            
    payloads = []
    for date_str, sensors in grouped.items():
        # Require buffer readings to form a valid heating record
        t_bottom = sensors.get("bufor_bottom")
        t_mid2 = sensors.get("bufor_mid2")
        t_mid1 = sensors.get("bufor_mid1")
        t_top = sensors.get("bufor_top")
        
        if None in (t_bottom, t_mid2, t_mid1, t_top):
            continue
            
        wabt = (t_bottom * 52 + t_mid2 * 37 + t_mid1 * 34 + t_top * 46) / 169.0
        q_buf = max(0.0, 0.696 * wabt - 17.43)
        
        t_piec = sensors.get("temp_piec", 0.0)
        t_co = sensors.get("temp_co", 0.0)
        
        # ISO timestamp format for BigQuery
        ts = datetime.strptime(date_str, "%Y-%m-%d %H:%M:%S").replace(tzinfo=timezone.utc)
        
        payloads.append({
            "installation_id": INSTALLATION_ID,
            "timestamp": ts.isoformat(),
            "temp_piec": t_piec,
            "temp_co": t_co,
            "bufor_top": t_top,
            "bufor_mid1": t_mid1,
            "bufor_mid2": t_mid2,
            "bufor_bottom": t_bottom,
            "temp_home": sensors.get("temp_home"),
            "temp_ext": sensors.get("temp_ext"),
            "wabt": round(wabt, 2),
            "q_buf": round(q_buf, 2),
            "heating_cycle": 1 if t_piec > 55.0 else 0,
            "discharging_cycle": 1 if t_co > 28.0 else 0,
        })
        
    return payloads


def stream_to_bigquery(client: bigquery.Client, rows: List[Dict[str, Any]], batch_size: int = 500) -> int:
    """Stream records in batches to BigQuery."""
    total = len(rows)
    inserted = 0
    table = client.get_table(FULL_TABLE_REF)
    
    for i in range(0, total, batch_size):
        chunk = rows[i:i + batch_size]
        errors = client.insert_rows_json(table, chunk)
        if errors:
            raise RuntimeError(f"BigQuery streaming insert errors: {errors}")
        inserted += len(chunk)
        print(f"Streamed {inserted}/{total} rows to BigQuery...")
        
    return inserted


def main():
    print(f"[{datetime.now()}] Starting Domoticz -> BigQuery sync for {INSTALLATION_ID}...")
    client = get_bigquery_client()
    
    last_time = get_last_timestamp(client)
    if last_time:
        print(f"Last record in BigQuery: {last_time} UTC. Fetching new data...")
    else:
        print("No prior data in BigQuery for this installation. Initializing 7-day backfill...")
        
    records = fetch_domoticz_records(last_time)
    print(f"Prepared {len(records)} records from Domoticz.")
    
    if records:
        count = stream_to_bigquery(client, records)
        print(f"Successfully synced {count} rows to BigQuery ({FULL_TABLE_REF}).")
    else:
        print("BigQuery is already up to date. No new records to sync.")


if __name__ == "__main__":
    main()

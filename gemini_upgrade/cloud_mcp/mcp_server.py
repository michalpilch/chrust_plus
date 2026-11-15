#!/usr/bin/env python3
"""
FastMCP Server for Heating IQ Platform.
Connects to Google Cloud BigQuery and Weather Underground to provide tools
for Gemini LLM agents and web clients.
"""

import os
from typing import Any, Dict, List, Optional
import requests
from google.cloud import bigquery, secretmanager
from google.oauth2 import service_account
try:
    from mcp.server.fastmcp import FastMCP
except (ImportError, ModuleNotFoundError):
    try:
        from mcp.server.mcpserver import MCPServer as FastMCP
    except (ImportError, ModuleNotFoundError):
        class FastMCP:
            def __init__(self, name):
                self.name = name

            def tool(self):
                def decorator(f):
                    return f
                return decorator

            def run(self):
                pass

# ============================================================
# Configuration
# ============================================================
PROJECT_ID = "heating-iq-2026"
DATASET_ID = "heating_telemetry"
SECRET_ID = "WUNDERGROUND_API_KEY"
DEFAULT_INSTALLATION_ID = "chrust_plus_01"

KEY_FILE = os.environ.get(
    "GOOGLE_APPLICATION_CREDENTIALS",
    "/home/dsl/.config/gcp/heating-iq-mcp-backend.json",
)

mcp = FastMCP("HeatingIQ-CloudMCP")


def get_bigquery_client() -> bigquery.Client:
    """Create BigQuery client using service account credentials."""
    if os.path.exists(KEY_FILE):
        credentials = service_account.Credentials.from_service_account_file(
            KEY_FILE,
            scopes=["https://www.googleapis.com/auth/bigquery"],
        )
        return bigquery.Client(project=PROJECT_ID, credentials=credentials, location="europe-central2")
    return bigquery.Client(project=PROJECT_ID, location="europe-central2")


def get_wunderground_api_key() -> str:
    """Read Weather Underground API key from Google Secret Manager."""
    client = secretmanager.SecretManagerServiceClient()
    name = f"projects/{PROJECT_ID}/secrets/{SECRET_ID}/versions/latest"
    response = client.access_secret_version(request={"name": name})
    return response.payload.data.decode("utf-8").strip()


# ============================================================
# MCP Tools
# ============================================================

@mcp.tool()
def get_installation_parameters(installation_id: str = DEFAULT_INSTALLATION_ID) -> Dict[str, Any]:
    """
    Get boiler, buffer, fuel and network configuration for a given installation.
    """
    try:
        client = get_bigquery_client()
        query = f"""
            SELECT *
            FROM `{PROJECT_ID}.{DATASET_ID}.installations`
            WHERE installation_id = @inst_id
            LIMIT 1
        """
        job_config = bigquery.QueryJobConfig(
            query_parameters=[bigquery.ScalarQueryParameter("inst_id", "STRING", installation_id)]
        )
        results = list(client.query(query, job_config=job_config).result())
        if not results:
            return {"error": f"Installation '{installation_id}' not found in BigQuery."}
        row = dict(results[0].items())
        if "created_at" in row and row["created_at"]:
            row["created_at"] = str(row["created_at"])
        return {"status": "SUCCESS", "installation": row}
    except Exception as e:
        return {"error": "Failed to fetch installation parameters.", "details": str(e)}


@mcp.tool()
def get_latest_telemetry(installation_id: str = DEFAULT_INSTALLATION_ID) -> Dict[str, Any]:
    """
    Get the most recent temperature and buffer energy telemetry for an installation from BigQuery.
    """
    try:
        client = get_bigquery_client()
        query = f"""
            SELECT *
            FROM `{PROJECT_ID}.{DATASET_ID}.sensor_readings`
            WHERE installation_id = @inst_id
            ORDER BY timestamp DESC
            LIMIT 1
        """
        job_config = bigquery.QueryJobConfig(
            query_parameters=[bigquery.ScalarQueryParameter("inst_id", "STRING", installation_id)]
        )
        results = list(client.query(query, job_config=job_config).result())
        if not results:
            return {"error": f"No telemetry found for installation '{installation_id}'."}
        row = dict(results[0].items())
        if "timestamp" in row and row["timestamp"]:
            row["timestamp"] = str(row["timestamp"])
        return {"status": "SUCCESS", "telemetry": row}
    except Exception as e:
        return {"error": "Failed to fetch latest telemetry.", "details": str(e)}


@mcp.tool()
def get_telemetry_history(
    installation_id: str = DEFAULT_INSTALLATION_ID,
    hours: int = 12,
) -> Dict[str, Any]:
    """
    Get temperature and energy history over the last N hours for trend analysis.
    """
    try:
        hours = max(1, min(int(hours), 168))
        client = get_bigquery_client()
        query = f"""
            SELECT 
              timestamp, temp_piec, temp_co, bufor_top, bufor_mid1, bufor_mid2, bufor_bottom,
              temp_home, temp_ext, wabt, q_buf, heating_cycle, discharging_cycle
            FROM `{PROJECT_ID}.{DATASET_ID}.sensor_readings`
            WHERE installation_id = @inst_id
              AND timestamp >= TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL @hours HOUR)
            ORDER BY timestamp ASC
        """
        job_config = bigquery.QueryJobConfig(
            query_parameters=[
                bigquery.ScalarQueryParameter("inst_id", "STRING", installation_id),
                bigquery.ScalarQueryParameter("hours", "INT64", hours),
            ]
        )
        results = client.query(query, job_config=job_config).result()
        readings = []
        for r in results:
            item = dict(r.items())
            if "timestamp" in item and item["timestamp"]:
                item["timestamp"] = str(item["timestamp"])
            readings.append(item)
        return {
            "status": "SUCCESS",
            "installation_id": installation_id,
            "hours": hours,
            "count": len(readings),
            "readings": readings,
        }
    except Exception as e:
        return {"error": "Failed to fetch telemetry history.", "details": str(e)}


@mcp.tool()
def calculate_wood_demand(
    installation_id: str = DEFAULT_INSTALLATION_ID,
    target_wabt_c: Optional[float] = None,
) -> Dict[str, Any]:
    """
    Calculate the exact required fuel/wood load [kg] for a burn cycle based on current physical telemetry.

    Considers:
    - Safe upper target WABT (default 70°C from installation profile to prevent boiler boiling)
    - Active house heating loss during the 2.0h burn cycle
    - Thermal mass of 200L central heating pipe loop from cold-start state
    - Fuel heating value and boiler efficiency
    """
    try:
        # 1. Fetch installation parameters
        inst_res = get_installation_parameters(installation_id)
        if "error" in inst_res:
            return inst_res
        inst = inst_res["installation"]

        # 2. Fetch latest telemetry
        telem_res = get_latest_telemetry(installation_id)
        if "error" in telem_res:
            return telem_res
        telem = telem_res["telemetry"]

        # Parameters
        safe_wabt_target = target_wabt_c if target_wabt_c is not None else float(inst.get("target_wabt_c", 70.0))
        buffer_vol_l = float(inst.get("buffer_volume_l", 600.0))
        network_vol_l = float(inst.get("network_volume_l", 200.0))
        fuel_kwh_per_kg = float(inst.get("fuel_kwh_per_kg", 3.65))
        fuel_type = inst.get("fuel_type", "dry_beech_wood")

        # Current telemetry values
        current_wabt = float(telem.get("wabt", 30.0))
        current_q_buf = float(telem.get("q_buf", 0.0))
        t_ext = float(telem.get("temp_ext", 0.0)) if telem.get("temp_ext") is not None else 0.0
        t_co = float(telem.get("temp_co", 25.0)) if telem.get("temp_co") is not None else 25.0

        # Buffer energy deficit
        # Target Q_buf at safe_wabt_target (e.g. 70C -> 31.29 kWh for 600L)
        q_target = max(0.0, (buffer_vol_l / 600.0) * (0.696 * safe_wabt_target - 17.43))
        delta_q_buf = max(0.0, q_target - current_q_buf)

        if delta_q_buf <= 0.0:
            return {
                "status": "BUFFER_SATURATED",
                "message": f"Buffer WABT is {current_wabt:.1f}°C (>= {safe_wabt_target:.1f}°C). Boiler at target capacity. Do not load wood.",
                "recommended_wood_kg": 0.0,
                "current_wabt_c": round(current_wabt, 1),
            }

        # House heat demand during 2.0h burn cycle
        p_house = max(0.0, 3.4085 - 0.1228 * t_ext) if t_ext < 16.0 else 0.0
        q_house = p_house * 2.0

        # Network heat-up (200L)
        t_co_target = 35.03 - 0.604 * t_ext
        delta_t_co = max(0.0, t_co_target - t_co)
        c_net = network_vol_l * 4.186 / 3600.0  # 0.2326 kWh/K for 200L
        q_network = c_net * delta_t_co

        # Total energy needed & wood mass
        q_total = delta_q_buf + q_house + q_network
        wood_kg = round(q_total / fuel_kwh_per_kg, 1)

        return {
            "status": "SUCCESS",
            "installation_id": installation_id,
            "recommended_wood_kg": wood_kg,
            "fuel": f"{fuel_type} ({fuel_kwh_per_kg:.2f} kWh_eff/kg)",
            "safe_target_wabt_c": safe_wabt_target,
            "current_state": {
                "wabt_c": round(current_wabt, 1),
                "q_buf_kwh": round(current_q_buf, 2),
                "temp_co_c": round(t_co, 1),
                "temp_co_target_c": round(t_co_target, 1),
                "temp_ext_c": round(t_ext, 1),
                "boiler_temp_c": round(float(telem.get("temp_piec", 0.0)), 1),
            },
            "energy_breakdown_kwh": {
                "delta_buffer_kwh": round(delta_q_buf, 2),
                "house_consumption_during_burn_kwh": round(q_house, 2),
                "network_heatup_kwh": round(q_network, 2),
                "total_energy_kwh": round(q_total, 2),
            },
        }
    except Exception as e:
        return {"error": "Calculation failed.", "details": str(e)}


@mcp.tool()
def check_dhw_status(installation_id: str = DEFAULT_INSTALLATION_ID) -> Dict[str, Any]:
    """
    Check Domestic Hot Water (DHW / CWU) availability for showers or baths.

    The buffer tank contains an internal stainless-steel/copper flow-through coil
    located in the UPPER zone of the buffer. Because water is stratified, DHW
    availability is determined primarily by bufor_top and bufor_mid1, NOT by
    the average buffer temperature (WABT).

    Returns:
    - can_take_shower: bool (True if comfortable shower is possible)
    - comfort_level: high / medium / no hot water
    - estimated_water_temp_c: estimated domestic water output temp
    - estimated_showers_available: approximate number of 45-liter showers
    - advice: clear recommendation for user
    """
    try:
        inst_res = get_installation_parameters(installation_id)
        if "error" in inst_res:
            return inst_res
        inst = inst_res["installation"]

        has_coil = inst.get("has_dhw_coil", True)
        min_temp = float(inst.get("dhw_min_temp_c", 45.0))

        telem_res = get_latest_telemetry(installation_id)
        if "error" in telem_res:
            return telem_res
        telem = telem_res["telemetry"]

        t_top = float(telem.get("bufor_top", 0.0))
        t_mid1 = float(telem.get("bufor_mid1", 0.0))
        wabt = float(telem.get("wabt", 0.0))

        estimated_tap_temp = round(max(10.0, t_top - 3.5), 1)

        t_upper_avg = (t_top + t_mid1) / 2.0
        usable_energy_kwh = max(0.0, 250.0 * 4.186 * (t_upper_avg - 38.0) / 3600.0)
        available_showers = int(usable_energy_kwh / 1.5) if t_top >= 42.0 else 0

        if t_top >= min_temp + 3.0:
            can_shower = True
            comfort = "KOMFORT WYSOKI (gorąca woda pod dostatkiem)"
            advice = f"Można bez obaw brać długi prysznic lub kąpiel. Woda na wylewce osiągnie ok. {estimated_tap_temp}°C."
        elif t_top >= min_temp - 3.0:
            can_shower = True
            comfort = "KOMFORT UMIARKOWANY (krótki, ekonomiczny prysznic)"
            advice = f"Ciepła woda jest dostępna (ok. {estimated_tap_temp}°C), ale bufor w górnej strefie jest na wyczerpaniu. Zalecany szybki prysznic."
        else:
            can_shower = False
            comfort = "BRAK CIEPŁEJ WODY (woda letnia lub zimna)"
            advice = f"Temperatura góry bufora ({t_top:.1f}°C) jest zbyt niska na prysznic. Woda na wylewce będzie miała poniżej 38°C. Należy rozpalić w kotle."

        return {
            "status": "SUCCESS",
            "installation_id": installation_id,
            "has_dhw_coil": has_coil,
            "can_take_shower": can_shower,
            "comfort_level": comfort,
            "bufor_top_c": round(t_top, 1),
            "bufor_mid1_c": round(t_mid1, 1),
            "wabt_average_c": round(wabt, 1),
            "estimated_water_temp_c": estimated_tap_temp,
            "estimated_showers_available": available_showers,
            "advice": advice,
        }
    except Exception as e:
        return {"error": "Failed to check DHW status.", "details": str(e)}


@mcp.tool()
def get_weather(station_id: str = "IROGOZ2") -> Dict[str, Any]:
    """Get current weather conditions from Weather Underground station."""
    try:
        api_key = get_wunderground_api_key()
        url = "https://api.weather.com/v2/pws/observations/current"
        params = {
            "stationId": station_id,
            "format": "json",
            "units": "s",
            "numericPrecision": "decimal",
            "apiKey": api_key,
        }
        res = requests.get(url, params=params, timeout=10)
        res.raise_for_status()
        obs = res.json()["observations"][0]
        metric = obs["metric_si"]
        return {
            "station": obs.get("stationID"),
            "location": obs.get("neighborhood"),
            "temperature_c": metric.get("temp"),
            "humidity_percent": obs.get("humidity"),
            "wind_speed_kmh": metric.get("windSpeed"),
            "pressure_hpa": metric.get("pressure"),
            "wind_chill_c": metric.get("windChill"),
        }
    except Exception as e:
        return {"error": "Weather API failed.", "details": str(e)}


if __name__ == "__main__":
    mcp.run()

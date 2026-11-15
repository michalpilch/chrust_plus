#!/usr/bin/env python3
"""
Heating IQ Platform - Client Dashboard & Gemini AI Engineering Assistant.
Multi-tenant, BigQuery-backed web application.
"""

import os
import sys
from datetime import datetime
import pandas as pd
import streamlit as st

# Ensure cloud_mcp directory is in path
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import mcp_server

from google import genai
from google.genai import types

# Page config
st.set_page_config(
    page_title="Heating IQ - Inteligentny Asystent Kotłowni",
    page_icon="🔥",
    layout="wide",
)

# Load Gemini API Key
KEY_ENV_FILE = "/home/dsl/.config/gcp/gemini.env"
if "GEMINI_API_KEY" not in os.environ and os.path.exists(KEY_ENV_FILE):
    with open(KEY_ENV_FILE) as f:
        for line in f:
            if line.startswith("GEMINI_API_KEY="):
                os.environ["GEMINI_API_KEY"] = line.strip().split("=", 1)[1]


@st.cache_resource
def get_gemini_client():
    return genai.Client()


client = get_gemini_client()

# ============================================================
# Sidebar: Installation selector
# ============================================================
st.sidebar.image("https://img.icons8.com/fluency/96/fire-element.png", width=64)
st.sidebar.title("Heating IQ Platform")
st.sidebar.caption("System Zarządzania i Optymalizacji Spalania")

installation_id = st.sidebar.selectbox(
    "Wybierz instalację:",
    options=["chrust_plus_01"],
    format_func=lambda x: f"{x} (HappySt8 - Rogozino)",
)

# Fetch installation parameters & telemetry
inst_data = mcp_server.get_installation_parameters(installation_id)
telem_data = mcp_server.get_latest_telemetry(installation_id)
wood_calc = mcp_server.calculate_wood_demand(installation_id)
dhw_status = mcp_server.check_dhw_status(installation_id)

inst = inst_data.get("installation", {})
telem = telem_data.get("telemetry", {})

if st.sidebar.button("🔄 Odśwież dane"):
    st.rerun()

st.sidebar.markdown("---")
st.sidebar.markdown(f"**Klient:** {inst.get('client_name', 'N/A')}")
st.sidebar.markdown(f"**Lokalizacja:** {inst.get('city', 'N/A')}")
st.sidebar.markdown(f"**Kocioł:** {inst.get('boiler_type', 'N/A')} ({inst.get('boiler_power_kw', 14)} kW)")
st.sidebar.markdown(f"**Bufor:** {inst.get('buffer_volume_l', 600)} L (cel WABT: {inst.get('target_wabt_c', 70)}°C)")
st.sidebar.markdown(f"**Paliwo:** {inst.get('fuel_type', 'suchy buk')}")

# ============================================================
# Top KPIs & Wood Recommendation
# ============================================================
st.title(f"🔥 Panel Kotłowni: {inst.get('client_name', 'Instalacja')} ({installation_id})")
st.caption(f"Ostatni odczyt telemetryczny: {telem.get('timestamp', 'Brak danych')} UTC (BigQuery: europe-central2)")

col1, col2, col3, col4, col5 = st.columns(5)

with col1:
    rec_wood = wood_calc.get("recommended_wood_kg", 0.0)
    st.metric(
        label="🪵 Sugerowany wsad drewna",
        value=f"{rec_wood} kg",
        help="Obliczone z bilansu: doładowanie bufora do 70°C + straty domu w 2h + wygrzanie sieci 200L.",
    )

with col2:
    can_shower = dhw_status.get("can_take_shower", False)
    t_tap = dhw_status.get("estimated_water_temp_c", 0.0)
    showers_left = dhw_status.get("estimated_showers_available", 0)
    st.metric(
        label="🚿 Prysznic / Ciepła woda",
        value="Dostępny" if can_shower else "Zimna woda",
        delta=f"Woda ~{t_tap}°C ({showers_left} prysznice)",
        delta_color="normal" if can_shower else "inverse",
        help="Ciepła woda jest grzana przepływowo przez wężownicę w buforze. Zależy głównie od bufor_top.",
    )

with col3:
    wabt = telem.get("wabt", 0.0)
    q_buf = telem.get("q_buf", 0.0)
    st.metric(
        label="💧 Średnia bufora (WABT)",
        value=f"{wabt:.1f} °C",
        delta=f"{q_buf:.1f} kWh zgromadzone",
    )

with col4:
    t_piec = telem.get("temp_piec", 0.0)
    st.metric(
        label="🔥 Temperatura kotła",
        value=f"{t_piec:.1f} °C",
        delta="Praca aktywna" if t_piec > 55 else "Wygaszone / czuwanie",
        delta_color="normal" if t_piec > 55 else "off",
    )

with col5:
    t_ext = telem.get("temp_ext", 0.0)
    t_co = telem.get("temp_co", 0.0)
    st.metric(
        label="🏠 Zasilanie CO / Zewnątrz",
        value=f"{t_co:.1f} °C",
        delta=f"Zewnętrzna: {t_ext:.1f} °C",
    )

st.markdown("---")

# ============================================================
# Stratification & History Chart
# ============================================================
col_strat, col_chart = st.columns([1, 2])

with col_strat:
    st.subheader("Warstwy termiczne bufora")
    t_top = telem.get("bufor_top", 0.0)
    t_mid1 = telem.get("bufor_mid1", 0.0)
    t_mid2 = telem.get("bufor_mid2", 0.0)
    t_bottom = telem.get("bufor_bottom", 0.0)

    st.markdown(f"🔴 **Góra (bufor_top):** `{t_top:.1f} °C`")
    st.progress(min(1.0, max(0.0, (t_top - 20) / 60)))

    st.markdown(f"🟠 **Środek 1 (bufor_mid1):** `{t_mid1:.1f} °C`")
    st.progress(min(1.0, max(0.0, (t_mid1 - 20) / 60)))

    st.markdown(f"🟡 **Środek 2 (bufor_mid2):** `{t_mid2:.1f} °C`")
    st.progress(min(1.0, max(0.0, (t_mid2 - 20) / 60)))

    st.markdown(f"🔵 **Dół (bufor_bottom):** `{t_bottom:.1f} °C`")
    st.progress(min(1.0, max(0.0, (t_bottom - 20) / 60)))

    if "energy_breakdown_kwh" in wood_calc:
        eb = wood_calc["energy_breakdown_kwh"]
        st.info(
            f"**Szczegóły bilansu:**\n"
            f"* Bufor do 70°C: `+{eb.get('delta_buffer_kwh')} kWh`\n"
            f"* Odbiór domu (2h): `+{eb.get('house_consumption_during_burn_kwh')} kWh`\n"
            f"* Rozgrzanie rur 200L: `+{eb.get('network_heatup_kwh')} kWh`\n"
            f"* Suma energii: `{eb.get('total_energy_kwh')} kWh`"
        )

with col_chart:
    st.subheader("Historia temperatur (ostatnie 12h z BigQuery)")
    hist = mcp_server.get_telemetry_history(installation_id, hours=12)
    readings = hist.get("readings", [])
    if readings:
        df = pd.DataFrame(readings)
        df["timestamp"] = pd.to_datetime(df["timestamp"])
        df = df.set_index("timestamp")
        st.line_chart(
            df[["temp_piec", "bufor_top", "bufor_bottom", "temp_co", "temp_ext"]],
            height=320,
        )
    else:
        st.write("Brak danych historycznych w zadanym oknie.")

st.markdown("---")

# ============================================================
# Gemini AI Assistant Chat
# ============================================================
st.subheader("💬 Asystent Inżynierski Gemini 3.8 Flash")
st.caption("Zapytaj asystenta o stan instalacji, prognozę zapotrzebowania, dobór opału lub anomalia.")

if "messages" not in st.session_state:
    st.session_state["messages"] = [
        {
            "role": "assistant",
            "content": "Dzień dobry! Jestem Twoim inżynierem kotłowni. Monitoruję dane z BigQuery oraz pogodę w Rogozinie. W czym mogę Ci pomóc?",
        }
    ]

for msg in st.session_state["messages"]:
    st.chat_message(msg["role"]).write(msg["content"])

if prompt := st.chat_input("Zadaj pytanie asystentowi (np. 'Czy muszę dzisiaj jeszcze dokładać drewna?')..."):
    st.session_state["messages"].append({"role": "user", "content": prompt})
    st.chat_message("user").write(prompt)

    system_prompt = f"""
    Jesteś profesjonalnym inżynierem i doradcą klienta w systemie Heating IQ Platform.
    Pracujesz z instalacją o ID: '{installation_id}'.

    KLUCZOWE ZASADY FIZYKI I INSTALACJI:
    1. CIEPŁA WODA UŻYTKOWA (DHW / CWU / PRYSZNIC):
       - W buforze 600L znajduje się przepływowa wężownica ze stali nierdzewnej umieszczona w GÓRNEJ części bufora.
       - Bufor jest uwarstwiony termicznie. Dlatego o dostępności ciepłej wody na prysznic decyduje wyłącznie temperatura w górnej strefie: `bufor_top` oraz `bufor_mid1`, a NIGDY średnia temperatura bufora (WABT)!
       - Gdy użytkownik pyta o prysznic, kąpiel lub ciepłą wodę do mycia: ZAWSZE wywołaj narzędzie `check_dhw_status`.
       - Kryteria:
         * bufor_top >= 48°C: Komfort wysoki, gorąca woda pod dostatkiem (można brać długi prysznic lub kąpiel).
         * 42°C <= bufor_top < 48°C: Komfort umiarkowany (krótki, ekonomiczny prysznic).
         * bufor_top < 42°C: Brak ciepłej wody (woda za zimna na prysznic, należy rozpalić w kotle).
    2. DREWNO I KOCIOŁ:
       - Używaj `calculate_wood_demand` do wyliczenia optymalnego wsadu (bezpieczny pułap WABT to 70°C).
    3. Zawsze odpowiadaj rzeczowo, uprzejmie i w języku polskim. Wyjaśniaj fizyczne przesłanki swoich rekomendacji.
    """

    with st.chat_message("assistant"):
        with st.spinner("Analizuję telemetrię i kalkulacje..."):
            chat = client.chats.create(
                model="gemini-3.8-flash",
                config=types.GenerateContentConfig(
                    system_instruction=system_prompt,
                    tools=[
                        mcp_server.get_installation_parameters,
                        mcp_server.get_latest_telemetry,
                        mcp_server.get_telemetry_history,
                        mcp_server.calculate_wood_demand,
                        mcp_server.check_dhw_status,
                        mcp_server.get_weather,
                    ],
                ),
            )
            resp = chat.send_message(prompt)
            reply = resp.text
            st.write(reply)
            st.session_state["messages"].append({"role": "assistant", "content": reply})

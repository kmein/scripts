import pandas as pd
import requests
import requests_cache

from collections import Counter
from typing import List, TypedDict
import locale

locale.setlocale(locale.LC_ALL, "de_DE")

requests_cache.install_cache("stoepel_cache")


class DistrictOrState(TypedDict):
    name: str
    key: str
    population: int
    path: str


DISTRICTS_URL = "https://geogen.stoepel.net/content/de/districts.json"
STATES_URL = "https://geogen.stoepel.net/content/de/states.json"
DISTRICT_CLUSTER_URL = "https://geogen.stoepel.net/api/clusters/district"

unlines = "\n".join


def get_districts() -> List[DistrictOrState]:
    return requests.get(DISTRICTS_URL).json()


def get_states() -> List[DistrictOrState]:
    return requests.get(STATES_URL).json()


def get_name_info(names: List[str]) -> Counter:
    count: Counter = Counter()
    for name in names:
        response = requests.get(DISTRICT_CLUSTER_URL, params={"name": name}).json()
        key_value_list = response["clusterers"]["DistrictClusterer"]["Data"]
        count += Counter(dict(key_value_list))
    return count


def create_data_frame(names: List[str]) -> pd.DataFrame:
    df = pd.DataFrame.from_dict(get_districts())
    df["absolute"] = df["key"].map(get_name_info(names)).fillna(0)
    df["relative"] = 1_000_000 * (df["absolute"] / df["population"])
    return df


def generate_map(df, key, fill_color):
    state_paths = []
    for state in get_states():
        state_paths.append(
            f'<path stroke="black" fill="none" d="{state["path"].strip()}"/>'
        )
    district_paths = []
    for _, district in df.iterrows():
        district_paths.append(
            f'<path stroke="darkgrey" fill="{fill_color}" fill-opacity="{round(district[key] / df[key].max(), 3)}" d="{district["path"].strip()}"><title>{district["name"]}: {locale.str(round(district[key], 2))}</title></path>'
        )
    return f"""<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg width="650" height="900">
  <defs>
    <linearGradient id="legend" x1="0" x2="1" y1="0" y2="0">
      <stop offset="0%" stop-color="white"/>
      <stop offset="100%" stop-color="{fill_color}"/>
    </linearGradient>
  </defs>
  <g>
    <rect x="150" y="880" stroke="darkgrey" width="350" height="10" fill="url(#legend)"/>
    <text x="510" y="890" font-size="18" fill="black">{locale.str(round(df[key].max(),2))}</text>
    <text x="135" y="890" font-size="18" fill="black">0</text>
  </g>
  <g>{unlines(district_paths)}</g>
  <g>{unlines(state_paths)}</g>
</svg>"""

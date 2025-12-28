{ writers, python3Packages }:
writers.writePython3Bin "pun_sort_api.py" {
  flakeIgnore = [
    "E203"
    "E203"
    "E226"
    "E265"
    "E302"
    "E305"
    "E501"
    "F401"
    "F841"
    "W503"
  ];
  libraries = [
    python3Packages.fastapi
    python3Packages.uvicorn
    python3Packages.pydantic
    python3Packages.pydantic-core
  ];
} ./sort_api.py

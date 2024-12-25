import requests
import os
from pathlib import Path


def get_input(year, day):
    project_root = _find_root()
    input_file_path = Path(project_root) / f"inputs/day{day:02}.txt"

    if input_file_path.exists():
        return input_file_path.read_text()
    else:
        input_data = _download_input(year, day)
        input_file_path.parent.mkdir(parents=True, exist_ok=True)
        input_file_path.write_text(input_data)
        return input_data


def _download_input(year, day):
    project_root = _find_root()

    cookie_file_path = Path(project_root) / ".cookie"
    session_cookie = cookie_file_path.read_text().strip()

    url = f"https://adventofcode.com/{year}/day/{day}/input"
    cookies = {"session": session_cookie}
    response = requests.get(url, cookies=cookies)

    response.raise_for_status()
    return response.text


def _find_root():
    start_path = os.path.dirname(os.path.abspath(__file__))
    current_path = start_path
    while current_path != os.path.dirname(current_path):
        if "pyproject.toml" in os.listdir(current_path):
            return current_path
        current_path = os.path.dirname(current_path)
    raise FileNotFoundError("pyproject.toml not found")

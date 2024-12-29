import shutil
from pathlib import Path
import subprocess
import sys
import webbrowser
from datetime import datetime


def _maybe_poetry(input: list):
    if sys.prefix == sys.base_prefix:
        return ["poetry", "run"] + input
    else:
        # already in virtualenv: https://stackoverflow.com/a/1883251
        return input


def copy_template():
    project_root = Path(__file__).parent
    template_file = project_root / "_template.py"

    # Find the next dayXX.py file name
    day_files = sorted(project_root.glob("day*.py"))
    if day_files:
        last_day_file = day_files[-1]
        last_day_number = int(last_day_file.stem[3:])
        next_day_number = last_day_number + 1
    else:
        next_day_number = 1

    next_day_file = project_root / f"day{next_day_number:02}.py"

    # Copy the template file to the next day file
    shutil.copy(template_file, next_day_file)

    # Replace __DAY__ placeholder with the actual day number
    content = next_day_file.read_text()
    content = content.replace("__DAY__", str(next_day_number))
    next_day_file.write_text(content, newline="\n")

    print(f"Created {next_day_file}")


def _get_latest():
    project_root = Path(__file__).parent
    # Find the latest dayXX.py file
    day_files = sorted(project_root.glob("day*.py"))
    if not day_files:
        print("No dayXX.py files found.")
        return None

    return day_files[-1]


def run_latest():
    latest_day_file = _get_latest()
    if not latest_day_file:
        return

    subprocess.run(_maybe_poetry(["python", latest_day_file]))


def test_latest():
    latest_day_file = _get_latest()
    if not latest_day_file:
        return

    subprocess.run(_maybe_poetry(["pytest", latest_day_file]))


def open_web():
    latest_day_file = _get_latest()
    if not latest_day_file:
        return

    latest_day_number = int(latest_day_file.stem[3:])

    current_day = latest_day_number

    # Open the browser to the AoC puzzle
    url = f"https://adventofcode.com/2016/day/{current_day}"
    print(f"opening {url} ...")
    webbrowser.open(url)

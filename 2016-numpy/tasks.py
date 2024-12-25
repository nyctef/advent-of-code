import shutil
from pathlib import Path
import subprocess


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


def run_latest():
    project_root = Path(__file__).parent

    # Find the latest dayXX.py file
    day_files = sorted(project_root.glob("day*.py"))
    if not day_files:
        print("No dayXX.py files found.")
        return

    latest_day_file = day_files[-1]

    # Run the latest dayXX.py file
    subprocess.run(["poetry", "run", "python", latest_day_file])

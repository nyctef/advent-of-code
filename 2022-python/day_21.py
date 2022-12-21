from itertools import chain
from pathlib import Path
import re


input_file = Path("input/21-1.txt").read_text()

input = input_file.splitlines()
lefts = [x.split(":")[0] for x in input]
lefts = [x for x in lefts if x != "root"]

print(f"{len(lefts)=}")
rights = [x.split(":")[1] for x in input]
rights = [re.match(r"(\w{4})...(\w{4})", x.strip()) for x in rights]
rights = [(x.group(1), x.group(2)) for x in rights if x is not None]
rights = list(chain.from_iterable(rights))
print(f"{rights[0: 10]=}")
print(len(rights))

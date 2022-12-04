from pprint import pprint
import re


input_file = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"""

with open("input/4-1.txt") as f:
    input_file = f.read()

input_lines = input_file.splitlines()

inputs = [re.split(r"[,\-]", x) for x in input_lines]

count = 0

for [e1_low, e1_high, e2_low, e2_high] in inputs:
    e1_low = int(e1_low)
    e2_low = int(e2_low)
    e1_high = int(e1_high)
    e2_high = int(e2_high)
    print([e1_low, e1_high, e2_low, e2_high])
    if e1_low <= e2_low and e1_high >= e2_high:
        print("e1")
        count += 1
    elif e2_low <= e1_low and e2_high >= e1_high:
        print("e2")
        count += 1
    else:
        print("nope")

print(count)

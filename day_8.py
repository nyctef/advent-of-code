import itertools
from pprint import pprint


input_file = """30373
25512
65332
33549
35390
"""

with open("input/8-1.txt") as f:
    input_file = f.read()

print(input_file)

rows = input_file.splitlines()
width = len(rows[0])
height = len(rows)

result = [[0] * width for _ in range(height)]

# from the top
for c in range(width):
    threshold = -1
    for r in range(height):
        val = int(rows[r][c])
        if val > threshold:
            threshold = val
            result[r][c] |= 1
# from the left
for r in range(height):
    threshold = -1
    for c in range(width):
        val = int(rows[r][c])
        if val > threshold:
            threshold = val
            result[r][c] |= 1
# from the bottom
for c in range(width):
    threshold = -1
    for r in reversed(range(height)):
        val = int(rows[r][c])
        if val > threshold:
            threshold = val
            result[r][c] |= 1
# from the right
for r in range(height):
    threshold = -1
    for c in reversed(range(width)):
        val = int(rows[r][c])
        if val > threshold:
            threshold = val
            result[r][c] |= 1

pprint((width, height))
pprint(result)
pprint(sum(itertools.chain.from_iterable(result)))

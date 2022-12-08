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

# print(input_file)

rows = input_file.splitlines()
width = len(rows[0])
height = len(rows)

result = [[0] * width for _ in range(height)]


for start_r in range(1, height - 1):
    for start_c in range(1, width - 1):
        current_height = int(rows[start_r][start_c])
        print(f"looking at tree at ({start_r},{start_c}) with height {current_height}")
        up_trees = 0
        down_trees = 0
        left_trees = 0
        right_trees = 0
        for r in reversed(range(0, start_r)):
            up_trees += 1
            if int(rows[r][start_c]) >= current_height:
                break
        for r in range(start_r + 1, height):
            down_trees += 1
            if int(rows[r][start_c]) >= current_height:
                break
        for c in reversed(range(0, start_c)):
            left_trees += 1
            if int(rows[start_r][c]) >= current_height:
                break
        for c in range(start_c + 1, width):
            print(f"comp ({start_r},{c}): {int(rows[start_r][c])}")
            right_trees += 1
            if int(rows[start_r][c]) >= current_height:
                break
        print((up_trees, left_trees, down_trees, right_trees))
        result[start_r][start_c] = up_trees * down_trees * left_trees * right_trees


pprint((width, height))
pprint(result)
pprint(max(itertools.chain.from_iterable(result)))

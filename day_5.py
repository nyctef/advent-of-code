from pprint import pprint
import re


input_file = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

with open("input/5-1.txt") as f:
    input_file = f.read()

[crate_layout, steps] = input_file.split("\n\n")

crate_layout = crate_layout.splitlines()
num_stacks = int(crate_layout[-1].split()[-1])
# print(num_stacks)
crate_layout = crate_layout[0:-1]
crate_layout = list(reversed(crate_layout))

crates = [[] for x in range(0, num_stacks)]

for line in crate_layout:
    # print(line)
    for i in range(0, num_stacks):
        index = (i * 4) + 1
        char = line[index]
        # print((index, char))
        if char != " ":
            crates[i].append(char)

steps = steps.splitlines()
steps = [re.findall(r"\d+", x) for x in steps]

for [count, from_, to] in steps:
    print(crates)
    from_ = int(from_)
    to = int(to)
    count = int(count)
    from_ -= 1
    to -= 1
    for _ in range(count):
        c = crates[from_].pop()
        crates[to].append(c)

# pprint(crate_layout)
pprint(crates)
pprint(steps)

result = ""
for stack in crates:
    result += stack[-1]
print()
print(result)

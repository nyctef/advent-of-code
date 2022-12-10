input_file = """noop
addx 3
addx -5
"""

with open("input/10-1.txt") as f:
    input_file = f.read()


def parse(line: str):
    if line == "noop":
        return ("noop", 0)
    if line.startswith("addx"):
        x = line.split(" ")
        return (x[0], int(x[1]))
    raise Exception(f"parse {line=}")


instrs = input_file.splitlines()
instrs = [parse(x) for x in instrs]

clock = 0
x = 1
sum_strength = 0


def cycle():
    global clock
    clock += 1
    if clock in [20, 60, 100, 140, 180, 220]:
        strength = clock * x
        print(f"during cycle {clock=} {x=} {strength=}")
        global sum_strength
        sum_strength += strength


for (op, value) in instrs:
    if op == "noop":
        cycle()
    if op == "addx":
        cycle()
        cycle()
        x += value

print()
print(sum_strength)

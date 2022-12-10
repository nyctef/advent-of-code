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
# instrs = instrs[:15]

clock = 0
x = 1


def asdf(*args, **kwargs):
    pass


draw = asdf
log = print

result = ""


def cycle():
    global clock
    global x
    global result
    clock += 1
    log()
    log(f"during cycle {clock=} {x=}")
    if abs((clock % 40) - 1 - x) < 2:
        result += "#"
        log(f"CRT draws a #")
    else:
        result += "."
        log(f"CRT draws a .")
        pass

    if clock % 40 == 0:
        result += "\n"
        pass


for (op, value) in instrs:
    if op == "noop":
        log(f"start of instr: noop")
        cycle()
        log(f"end of instr: noop")
    if op == "addx":
        log(f"start of instr: addx {value}")
        cycle()
        cycle()
        x += value
        log(f"end of instr: addx {value} (x is now {x})")

# print()
# print(sum_strength)

print()
print(result)

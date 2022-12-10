from dataclasses import dataclass

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


def nothing(*args, **kwargs):
    pass


log = print  # or `nothing` to disable logging


@dataclass
class Machine:
    screen: str = ""
    clock: int = 0
    x: int = 1

    def cycle(self):
        self.clock += 1
        log()
        log(f"during cycle {self.clock=} {self.x=}")
        if abs((self.clock % 40) - 1 - self.x) < 2:
            self.screen += "#"
            log(f"CRT draws a #")
        else:
            self.screen += "."
            log(f"CRT draws a .")
            pass

        if self.clock % 40 == 0:
            self.screen += "\n"
            pass

    def noop(self):
        log(f"start of instr: noop")
        self.cycle()
        log(f"end of instr: noop")

    def addx(self, value):
        log(f"start of instr: addx {value}")
        self.cycle()
        self.cycle()
        self.x += value
        log(f"end of instr: addx {value} (x is now {self.x})")

    def render_frame(self):
        print(self.screen)


machine = Machine()
for (op, value) in instrs:
    if op == "noop":
        machine.noop()
    if op == "addx":
        machine.addx(value)

print()
machine.render_frame()

from pprint import pprint
import re


input_file = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"""

with open("input/11-1.txt") as f:
    input_file = f.read()

monkey_texts = input_file.split("\n\n")
monkeys = []
for monkey_text in monkey_texts:
    lines = [x.strip() for x in monkey_text.splitlines()]
    starting_items = lines[1].split(":")[1].strip().split(",")
    starting_items = [int(x) for x in starting_items]
    operation_text = lines[2].split(":")[1].strip().split(" ")
    match operation_text:
        case ["new", "=", "old", "*", "old"]:
            operation = ("OLDSQUARED",)
        case ["new", "=", "old", "*", val]:
            operation = ("OLDMUL", int(val))
        case ["new", "=", "old", "+", val]:
            operation = ("OLDPLUS", int(val))
        case _:
            raise Exception(operation_text)
    test_text = lines[3]
    match test_text.split(" "):
        case ["Test:", "divisible", "by", val]:
            divisible_by = int(val)
        case _:
            raise Exception(test_text)
    throw_to_if_true = int(re.search(r"\d+", lines[4]).group(0))
    throw_to_if_false = int(re.search(r"\d+", lines[5]).group(0))
    monkeys.append(
        (starting_items, operation, divisible_by, throw_to_if_true, throw_to_if_false)
    )

pprint(monkeys)


def apply_operation(op, value):
    match op[0]:
        case "OLDSQUARED":
            return value * value
        case "OLDMUL":
            return value * op[1]
        case "OLDPLUS":
            return value + op[1]
        case _:
            raise Exception(op)


inspection_counts = [0] * len(monkeys)

for round in range(1, 21):
    for (i, (items, op, div, ontrue, onfalse)) in enumerate(monkeys):
        for item in items:
            inspection_counts[i] += 1
            worry = apply_operation(op, item)
            worry //= 3
            if worry % div == 0:
                monkeys[ontrue][0].append(worry)
            else:
                monkeys[onfalse][0].append(worry)
        items.clear()

pprint(monkeys)
pprint(inspection_counts)

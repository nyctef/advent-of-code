from pprint import pprint


with open("input/1-1.txt") as f:
    input_file = f.read()

elfs = input_file.split("\n\n")
# pprint(elfs)
calories = []
for elf in elfs:
    x = elf.split()
    x = [int(x) for x in x]
    calories.append(sum(x))

print(sum(list(sorted(calories, reverse=True))[0:3]))

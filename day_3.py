from pprint import pprint


with open("input/3-1.txt") as f:
    input_file = f.read()

# input_file = """vJrwpWtwJgWrhcsFMMfFFhFp
# jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
# PmmdzqPrVvPwwTWBwg
# wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
# ttgJtRGJQctTZtZT
# CrZsJsPPZsGzwwsLwLmpwMDw
# """

rucksacks = input_file.splitlines()

groups = []
chunk = []
for i, rucksack in enumerate(rucksacks, start=1):
    chunk.append(rucksack)
    if i % 3 == 0:
        groups.append(chunk)
        chunk = []

pprint(groups)

common_items = []
for [a, b, c] in groups:
    common_item = next(iter(set.intersection(set(a), set(b), set(c))))
    common_items.append(common_item)

pprint(common_items)


def priority(char):
    if char.isupper():
        return ord(char) - 65 + 27
    else:
        return ord(char) - 97 + 1


priorities = [priority(x) for x in common_items]

print(sum(priorities))

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
compartments = [(x[0 : len(x) // 2], x[len(x) // 2 :]) for x in rucksacks]
matching_items = [
    next(iter(set.intersection(set(x[0]), set(x[1])))) for x in compartments
]


def priority(char):
    if char.isupper():
        return ord(char) - 65 + 27
    else:
        return ord(char) - 97 + 1


as_ints = [priority(x) for x in matching_items]

pprint(matching_items)
pprint(as_ints)

print(sum(as_ints))

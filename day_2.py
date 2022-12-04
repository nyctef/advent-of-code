from pprint import pprint


def convert(char):
    return {"A": "R", "B": "P", "C": "S", "X": "R", "Y": "P", "Z": "S"}[char]


def individual_score(strat):
    return {"R": 1, "P": 2, "S": 3}[strat]


def battle_score(opp, me):
    if opp == me:
        return 3
    beats = {"R": "S", "P": "R", "S": "P"}
    if beats[me] == opp:
        return 6
    return 0


with open("input/2-1.txt") as f:
    input_file = f.read()

lines = input_file.splitlines()
strats = []
for line in lines:
    [opp, me] = line.split()
    strats.append((convert(opp), convert(me)))
pprint(strats)

total_score = 0
for (opp, me) in strats:
    total_score += individual_score(me) + battle_score(opp, me)

print(total_score)

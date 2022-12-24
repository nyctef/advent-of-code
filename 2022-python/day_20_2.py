from pathlib import Path


def read_input(name: str):
    match name:
        case "example1":
            return """1
2
-3
3
-2
0
4
"""
        case "puzzle":
            return Path("input/20-1.txt").read_text()
        case other:
            raise Exception(other)


def mix(parsed: list[int]):
    indexes = list(range(len(parsed)))
    for i in range(len(indexes)):
        # print(indexes)
        ii = next(ii for ii, x in enumerate(indexes) if x == i)
        distance = parsed[i]
        # print(f"{i=} {ii=} {distance=}")
        del indexes[ii]
        indexes.insert((ii + distance) % len(indexes), i)

    mixed_orig = [parsed[i] for i in indexes]
    zero_loc = next(i for i, x in enumerate(mixed_orig) if x == 0)
    num1 = mixed_orig[(zero_loc + 1000) % len(mixed_orig)]
    num2 = mixed_orig[(zero_loc + 2000) % len(mixed_orig)]
    num3 = mixed_orig[(zero_loc + 3000) % len(mixed_orig)]
    print(f"{num1=} {num2=} {num3=} {num1+num2+num3=}")


def main(name: str):
    input_file = read_input(name)
    parsed = [int(x) for x in input_file.splitlines()]
    mixed = mix(parsed)
    print(mixed)


main("puzzle")

from pathlib import Path


def read_input(name: str):
    match name:
        case "example":
            return """1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"""
        case "puzzle":
            return Path("input/25-1.txt").read_text()
        case other:
            raise Exception(other)


lookup = {"=": -2, "-": -1, "0": 0, "1": 1, "2": 2}


def snafu_to_dec(x: str):
    total = 0
    e = 1
    for i in reversed(range(len(x))):
        total += e * (lookup[x[i]])
        e *= 5
    return total


def add_snafu(a: str, b: str):
    pass


def parse_input(input_file: str):
    lines = input_file.splitlines()
    lines = [snafu_to_dec(x) for x in lines]
    print(lines)


def main():
    i = read_input("example")
    print(parse_input(i))


main()

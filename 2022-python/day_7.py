from pprint import pprint
import re
from typing import Iterator


input_file = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

with open("input/7-1.txt") as f:
    input_file = f.read()

input_lines = input_file.splitlines()

fs = dict()
fs["/"] = dict()
i = iter(input_lines)


def create_fs(folder: dict, line_iter: Iterator[str]):
    try:
        while True:
            line = next(line_iter)
            # print(f">{line}<")
            if line == "$ ls":
                continue
            elif line.startswith("dir "):
                dir_name = line[4:]
                folder[dir_name] = dict()
            elif line.startswith("$ cd "):
                dir_name = line[5:]
                if dir_name == "..":
                    return
                create_fs(folder[dir_name], line_iter)
            else:
                (size, name) = line.split(" ", maxsplit=2)
                size = int(size)
                folder[name] = size
    except StopIteration:
        return


create_fs(fs, i)

pprint(fs)

dir_sizes = dict()


def list_dir_sizes(name: str, folder: dict):
    current_dir_size = 0
    print(f"looking at dir {name}")
    for (k, v) in folder.items():
        if isinstance(v, int):
            print(f"{k} is a file with size {v}")
            current_dir_size += v
        else:
            print(f"recursing into dir {k}")
            current_dir_size += list_dir_sizes(f"{name}/{k}", v)
    # print(f"size of dir {name} is {current_dir_size}")
    print(f"dir {name} had total size {current_dir_size}")
    dir_sizes[name] = current_dir_size
    return current_dir_size


list_dir_sizes("fs", fs)
# pprint(dir_sizes)

result = 0
for (dir, size) in dir_sizes.items():
    if size <= 100000:
        # print((dir, size))
        result += size

print(result)

all_nums = re.findall(r"\d+", input_file)
all_nums = [int(x) for x in all_nums]
total_size_sanity_check = sum(all_nums)
print(f"sanity check: {total_size_sanity_check} ")

total_space = 70000000
space_required = 30000000
overage = total_size_sanity_check - (total_space - space_required)
print(f"{overage=}")
candidate_dirs = [x[1] for x in dir_sizes.items() if x[1] > overage]
pprint(candidate_dirs)
print(min(candidate_dirs))

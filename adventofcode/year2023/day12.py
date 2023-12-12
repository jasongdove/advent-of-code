from adventofcode import Day
from collections import deque
import re


class Day12(Day):
    def __init__(self):
        super().__init__(2023, 12)
        self.reg = re.compile('(#+)')
        self.bin_map = {'0': '.', '1': '#'}

    def test_line(self, line: str, cont: list[int]) -> bool:
        sizes = self.reg.finditer(line)
        groups = [len(y) for y in [x.groups()[0] for x in sizes]]
        return groups == cont

    def solve_line(self, line: str) -> int:
        s = line.split()
        records = s[0]
        cont = list(map(int, s[1].split(',')))
        #print(records)
        #print(cont)
        q_count = records.count('?')
        #print(q_count)
        maybe_lines = []
        for i in range(0, 2**q_count):
            maybe_line = []
            binary = deque([self.bin_map[x] for x in bin(i)[2:].zfill(q_count)])
            for j in range(0, len(records)):
                if records[j] == '?':
                    maybe_line.append(binary.popleft())
                else:
                    maybe_line.append(records[j])
            maybe_lines.append("".join(maybe_line))
        return sum(1 if self.test_line(x, cont) else 0 for x in maybe_lines)

    def part01(self):
        text = super()._part01_input()
        arrangements = [self.solve_line(x) for x in text.splitlines()]
        return sum(arrangements)

    def part02(self):
        text = super()._part01_input()
        return 0

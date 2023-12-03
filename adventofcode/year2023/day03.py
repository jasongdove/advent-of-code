from adventofcode import Day
import re


class Day03(Day):
    def __init__(self):
        super().__init__(2023, 3)

    def part01(self):
        text = super()._part01_input()
        data = []
        values = []
        total = 0
        for line in text.splitlines():
            line_list = []
            for c in line:
                if c == '.':
                    line_list.append(None)
                else:
                    line_list.append(c)
            data.append(line_list)
        for row in data:
            ranges = []
            start = None
            finish = None
            for i in range(0, len(row)):
                if row[i] is not None and row[i].isdigit():
                    if start is None:
                        start = i
                        finish = i
                    else:
                        finish = i
                elif start is not None:
                    ranges.append((start, finish))
                    start = None
                    finish = None
            if start is not None and finish is not None:
                ranges.append((start, finish))
            for num_range in ranges:
                x0 = num_range[0]
                x1 = num_range[1]
                y = data.index(row)
                value = int("".join(row[x0:x1+1]))
                candidates = []
                for x in range(x0-1, x1 + 2):
                    candidates.append((x, y-1))
                    candidates.append((x, y+1))
                candidates.append((x0-1, y))
                candidates.append((x1+1, y))
                for candidate in candidates:
                    try:
                        spot = data[candidate[1]][candidate[0]]
                        if spot is not None:
                            values.append(value)
                            total += value
                            break
                    except IndexError:
                        continue
        #print(values)
        #print(total)
        return sum(values)

    def part02(self):
        text = super()._part01_input()
        return 0

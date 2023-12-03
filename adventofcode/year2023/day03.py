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
                value = int("".join(row[x0:x1 + 1]))
                candidates = []
                for x in range(x0 - 1, x1 + 2):
                    candidates.append((x, y - 1))
                    candidates.append((x, y + 1))
                candidates.append((x0 - 1, y))
                candidates.append((x1 + 1, y))
                for candidate in candidates:
                    try:
                        spot = data[candidate[1]][candidate[0]]
                        if spot is not None:
                            values.append(value)
                            total += value
                            break
                    except IndexError:
                        continue
        return sum(values)

    def part02(self):
        text = super()._part02_input()
        data = []
        values = []
        total = 0
        all_ranges = []
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
                    value = int("".join(row[start:finish + 1]))
                    ranges.append((start, finish, value))
                    start = None
                    finish = None
            if start is not None and finish is not None:
                value = int("".join(row[start:finish + 1]))
                ranges.append((start, finish, value))
            all_ranges.append(ranges)
        for row_number in range(0, len(data)):
            row = data[row_number]
            for col_number in range(0, len(row)):
                c = row[col_number]
                if c == '*':
                    adj = []
                    # check above
                    if row_number > 0:
                        ranges_above = all_ranges[row_number - 1]
                        for r in ranges_above:
                            s = set(range(r[0], r[1] + 1))
                            s2 = set(range(col_number - 1, col_number + 2))
                            if s.intersection(s2):
                                adj.append(r[2])
                    # check sides
                    ranges_this_row = all_ranges[row_number]
                    for r in ranges_this_row:
                        if r[1] == col_number - 1:
                            adj.append(r[2])
                        elif r[0] == col_number + 1:
                            adj.append(r[2])
                    # check below
                    if row_number < len(data):
                        ranges_below = all_ranges[row_number + 1]
                        for r in ranges_below:
                            s = set(range(r[0], r[1] + 1))
                            s2 = set(range(col_number - 1, col_number + 2))
                            if s.intersection(s2):
                                adj.append(r[2])
                    if len(adj) == 2:
                        print('match!!!!')
                        print(adj)
                        total += adj[0] * adj[1]
        return total

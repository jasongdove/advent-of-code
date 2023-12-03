from adventofcode import Day, Coordinate
import re


class PartNumber:

    def __init__(self, value, row_number, span):
        self.value = value
        self.coordinates = set(map(lambda col: Coordinate(row_number, col), range(span[0], span[1])))

    def to_check(self, data):
        max_row = len(data)
        max_col = len(data[0])
        result = set()
        for coord_set in map(lambda c: c.adj_diagonal(), self.coordinates):
            result.update(coord_set)
        result = set(filter(lambda c: 0 <= c.col < max_col and 0 <= c.row < max_row, result))
        return result.difference(self.coordinates)

    def __str__(self):
        return f'v: {self.value}, c: {self.coordinates}'


class Day03(Day):
    def __init__(self):
        super().__init__(2023, 3)
        self.reg = re.compile("(\\d+)")

    def part01(self):
        text = super()._part01_input()
        numbers = []
        data = [line for line in text.splitlines()]
        for row_number in range(0, len(data)):
            for num in self.reg.finditer(data[row_number]):
                numbers.append(PartNumber(int(num.group(0)), row_number, num.span()))
        total = 0
        for num in numbers:
            for check in num.to_check(data):
                if data[check.row][check.col] != '.':
                    total += num.value
        return total

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
                        # print(adj)
                        total += adj[0] * adj[1]
        return total

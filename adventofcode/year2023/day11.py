from adventofcode import Day, Coordinate
import queue


class Day11(Day):
    def __init__(self):
        super().__init__(2023, 11)

    @staticmethod
    def expand(the_map):
        # double empty rows
        clone = the_map[:]
        the_map.clear()
        for row in clone:
            if all(x == '.' for x in row):
                the_map.append(row)
            the_map.append(row)

        # double empty cols
        clone = the_map[:]
        the_map.clear()
        row_count = len(clone)
        to_expand = []
        for i in range(len(clone[0])):
            if all(clone[x][i] == '.' for x in range(row_count)):
                to_expand.append(i)

        for row in clone:
            next_row = []
            for i, c in enumerate(row):
                if i in to_expand:
                    next_row.append(c)
                next_row.append(c)
            the_map.append(next_row)

        return the_map

    def part01(self):
        text = super()._part01_input()
        the_map = [list(line) for line in text.splitlines()]
        the_map = Day11.expand(the_map)

        for row in the_map:
            print("".join(row))
        return 0

    def part02(self):
        text = super()._part01_input()
        return 0

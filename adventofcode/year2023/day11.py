from adventofcode import Day, Coordinate
from itertools import combinations


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

    @staticmethod
    def find_all_galaxies(the_map):
        galaxies = []
        for row_index, row in enumerate(the_map):
            for col_index, col in enumerate(row):
                if col == '#':
                    galaxies.append(Coordinate(row_index, col_index))
        return galaxies

    @staticmethod
    def min_distance(galaxy: Coordinate, galaxies: list[Coordinate]) -> int:
        result = 100000000
        for other in galaxies:
            if galaxy != other:
                distance = abs(galaxy.row - other.row) + abs(galaxy.col - other.col)
                result = min(distance, result)
        return result

    def part01(self):
        text = super()._part01_input()
        the_map = [list(line) for line in text.splitlines()]
        the_map = Day11.expand(the_map)

        #for row in the_map:
        #    print("".join(row))

        galaxies = Day11.find_all_galaxies(the_map)
        pairs = set(combinations(galaxies, 2))
        distance = 0
        for pair in pairs:
            #print(f'p0: {pair[0]}, p1: {pair[1]}')
            distance += abs(pair[0].row - pair[1].row) + abs(pair[0].col - pair[1].col)
        return distance

    def part02(self):
        text = super()._part01_input()
        return 0

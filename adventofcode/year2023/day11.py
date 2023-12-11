from adventofcode import Day, Coordinate
from itertools import combinations


class Day11(Day):
    def __init__(self):
        super().__init__(2023, 11)

    @staticmethod
    def get_expansion_rows(the_map: list[list[chr]]) -> list[int]:
        result = []
        for row_index, row in enumerate(the_map):
            if all(x == '.' for x in row):
                result.append(row_index)
        return result

    @staticmethod
    def get_expansion_cols(the_map: list[list[chr]]) -> list[int]:
        result = []
        for i in range(len(the_map[0])):
            if all(the_map[x][i] == '.' for x in range(len(the_map))):
                result.append(i)
        return result

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

    @staticmethod
    def solve(the_map: list[list[chr]], expansion_ratio: int) -> int:
        expansion_rows = Day11.get_expansion_rows(the_map)
        expansion_cols = Day11.get_expansion_cols(the_map)

        galaxies = Day11.find_all_galaxies(the_map)
        pairs = combinations(galaxies, 2)
        distance = 0
        for pair in pairs:
            min_row = min(pair[0].row, pair[1].row)
            max_row = max(pair[0].row, pair[1].row)
            min_col = min(pair[0].col, pair[1].col)
            max_col = max(pair[0].col, pair[1].col)

            extra_rows = sum([1 if min_row < r < max_row else 0 for r in expansion_rows])
            extra_cols = sum([1 if min_col < c < max_col else 0 for c in expansion_cols])

            max_row += extra_rows * (expansion_ratio - 1)
            max_col += extra_cols * (expansion_ratio - 1)

            distance += (max_row - min_row) + (max_col - min_col)
        return distance

    def part01(self):
        text = super()._part01_input()
        the_map = [list(line) for line in text.splitlines()]
        return Day11.solve(the_map, 2)

    def part02(self):
        text = super()._part01_input()
        the_map = [list(line) for line in text.splitlines()]
        return Day11.solve(the_map, 1_000_000)

import math
from collections import deque

from adventofcode import Day, Coordinate


class Day21(Day):
    def __init__(self):
        super().__init__(2023, 21)

    @staticmethod
    def find_start(the_map: list[list[chr]]) -> Coordinate:
        for row_index, row in enumerate(the_map):
            for column_index, col in enumerate(row):
                if col == 'S':
                    return Coordinate(row_index, column_index)

    @staticmethod
    def walk_steps(the_map: list[list[chr]], start: Coordinate, steps: int) -> list[Coordinate]:
        valid = ['.', 'S']
        q = deque([start])
        for _ in range(steps):
            next_q = set()
            while len(q) > 0:
                curr = q.popleft()
                for direction in [curr.up(), curr.left(), curr.down(), curr.right()]:
                    if direction.is_valid_in(the_map) and the_map[direction.row][direction.col] in valid:
                        next_q.add(direction)
            q.extend(next_q)
        return list(q)

    @staticmethod
    def walk_steps_infinite(the_map: list[list[chr]], start: Coordinate, steps: int) -> dict[int, int]:
        result = dict()
        row_len = len(the_map)
        col_len = len(the_map[0])
        valid = ['.', 'S']
        q = deque([start])
        for i in range(steps):
            next_q = set()
            while len(q) > 0:
                curr = q.popleft()
                for direction in [curr.up(), curr.left(), curr.down(), curr.right()]:
                    if the_map[direction.row % row_len][direction.col % col_len] in valid:
                        next_q.add(direction)
            q.extend(next_q)
            result[i + 1] = len(q)
        return result

    @staticmethod
    def newton(x, x1, y1, x2, y2, x3, y3):
        d21 = (y2 - y1) / (x2 - x1)
        d32 = (y3 - y2) / (x3 - x2)
        c = y1
        b = d21
        a = (d32 - d21) / (x3 - x1)
        return int(a * (x - x2) * (x - x1) + b * (x - x1) + c)

    def part01(self):
        text = super()._part01_input()
        the_map = [[c for c in line] for line in text.splitlines()]
        start = Day21.find_start(the_map)
        return len(Day21.walk_steps(the_map, start, 64))

    def part02(self):
        text = super()._part01_input()
        the_map = [[c for c in line] for line in text.splitlines()]
        start = Day21.find_start(the_map)

        target = 26501365
        map_length = len(the_map)
        r = target % map_length
        cached_walk = Day21.walk_steps_infinite(the_map, start, r + 2 * map_length)

        x1 = 0
        y1 = cached_walk[r]
        x2 = 1
        y2 = cached_walk[r + map_length]
        x3 = 2
        y3 = cached_walk[r + 2 * map_length]

        return Day21.newton(target // map_length, x1, y1, x2, y2, x3, y3)

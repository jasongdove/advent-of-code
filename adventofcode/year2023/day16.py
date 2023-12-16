from __future__ import annotations

from adventofcode import Day, Coordinate
from collections import deque


class Beam(Coordinate):
    def __init__(self, row: int, column: int, dx: int, dy: int):
        super().__init__(row, column)
        self.dx = dx
        self.dy = dy

    def as_coord(self) -> Coordinate:
        return Coordinate(self.row, self.col)

    def up(self) -> Beam:
        return Beam(self.row, self.col, 0, -1)

    def down(self) -> Beam:
        return Beam(self.row, self.col, 0, 1)

    def left(self) -> Beam:
        return Beam(self.row, self.col, -1, 0)

    def right(self) -> Beam:
        return Beam(self.row, self.col, 1, 0)

    def tick(self, the_map) -> list[Beam]:
        n = Beam(self.row + self.dy, self.col + self.dx, self.dx, self.dy)

        if not n.is_valid_in(the_map):
            return []

        match the_map[n.row][n.col]:
            case '.':
                return [n]
            case '|':
                if abs(self.dy) == 1:
                    return [n]
                else:
                    return [
                        n.up(),
                        n.down()
                    ]
            case '-':
                if abs(self.dx) == 1:
                    return [n]
                else:
                    return [
                        n.left(),
                        n.right()
                    ]
            case '/':
                if self.dx == 1:
                    return [n.up()]
                elif self.dx == -1:
                    return [n.down()]
                elif self.dy == 1:
                    return [n.left()]
                elif self.dy == -1:
                    return [n.right()]
            case '\\':
                if self.dx == 1:
                    return [n.down()]
                elif self.dx == -1:
                    return [n.up()]
                elif self.dy == 1:
                    return [n.right()]
                elif self.dy == -1:
                    return [n.left()]
            case _:
                print(f'done at {Coordinate(n.row, n.col)} => {the_map[n.row][n.col]}')
                return []

    def __eq__(self, other):
        if not isinstance(other, type(self)):
            return False
        return self.row == other.row and self.col == other.col and self.dx == other.dx and self.dy == other.dy

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash((self.row, self.col, self.dx, self.dy))

    def __str__(self):
        return f'({self.row}, {self.col}) d({self.dx}, {self.dy})'


def print_map(the_map, visited):
    for row_index, row in enumerate(the_map):
        to_print = ''
        for col_index, col in enumerate(row):
            if Coordinate(row_index, col_index) in visited:
                to_print += '#'
            else:
                to_print += col
        print(to_print)


class Day16(Day):
    def __init__(self):
        super().__init__(2023, 16)

    def part01(self):
        text = super()._part01_input()
        the_map = [[c for c in line] for line in text.splitlines()]
        beams = deque([Beam(0, -1, 1, 0)])
        visited = set()
        visited_c = set()
        while any(beams):
            beam = beams.popleft()
            to_q = beam.tick(the_map)

            for q in to_q:
                if q not in visited:
                    visited.add(q)
                    visited_c.add(q.as_coord())
                    beams.append(q)

        return len(visited_c)

    def part02(self):
        text = super()._part01_input()
        return 0

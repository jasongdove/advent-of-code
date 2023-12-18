from __future__ import annotations


class Coordinate:
    def __init__(self, row: int, col: int):
        self.row = row
        self.col = col

    def adj_diagonal(self) -> set[Coordinate]:
        return {
            Coordinate(self.row - 1, self.col - 1),
            Coordinate(self.row - 1, self.col),
            Coordinate(self.row - 1, self.col + 1),
            Coordinate(self.row, self.col - 1),
            Coordinate(self.row, self.col + 1),
            Coordinate(self.row + 1, self.col - 1),
            Coordinate(self.row + 1, self.col),
            Coordinate(self.row + 1, self.col + 1)
        }

    def left(self) -> Coordinate:
        return Coordinate(self.row, self.col - 1)

    def right(self) -> Coordinate:
        return Coordinate(self.row, self.col + 1)

    def up(self) -> Coordinate:
        return Coordinate(self.row - 1, self.col)

    def down(self) -> Coordinate:
        return Coordinate(self.row + 1, self.col)

    def is_valid_in(self, the_map) -> bool:
        return 0 <= self.row < len(the_map) and 0 <= self.col < len(the_map[0])

    def __eq__(self, other):
        return other and self.row == other.row and self.col == other.col

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash((self.row, self.col))

    def __str__(self):
        return f'({self.row}, {self.col})'

    def __repr__(self):
        return f'Coordinate({self.row}, {self.col})'
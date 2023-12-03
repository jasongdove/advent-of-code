class Coordinate:
    def __init__(self, row, col):
        self.row = row
        self.col = col

    def adj_diagonal(self):
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

    def __eq__(self, other):
        return other and self.row == other.row and self.col == other.col

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash((self.row, self.col))

    def __str__(self):
        return f'({self.col}, {self.row})'

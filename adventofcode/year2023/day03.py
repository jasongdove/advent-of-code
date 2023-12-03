from adventofcode import Day, Coordinate
import re


class Item:
    def __init__(self, coordinates):
        self.coordinates = coordinates

    def to_check(self, data):
        max_row = len(data)
        max_col = len(data[0])
        result = set()
        for coord_set in map(lambda c: c.adj_diagonal(), self.coordinates):
            result.update(coord_set)
        result = set(filter(lambda c: 0 <= c.col < max_col and 0 <= c.row < max_row, result))
        return result.difference(self.coordinates)


class PartNumber(Item):

    def __init__(self, value, row_number, span):
        self.value = value
        self.coordinates = set(map(lambda col: Coordinate(row_number, col), range(span[0], span[1])))
        super().__init__(self.coordinates)

    def __str__(self):
        return f'v: {self.value}, c: {self.coordinates}'


class Gear(Item):

    def __init__(self, row_number, span):
        self.coordinates = set(map(lambda col: Coordinate(row_number, col), range(span[0], span[1])))
        super().__init__(self.coordinates)


class Day03(Day):
    def __init__(self):
        super().__init__(2023, 3)
        self.num_reg = re.compile("(\\d+)")
        self.gear_reg = re.compile("(\*)")

    def numbers_from_data(self, data):
        result = []
        for row_number in range(0, len(data)):
            for num in self.num_reg.finditer(data[row_number]):
                result.append(PartNumber(int(num.group(0)), row_number, num.span()))
        return result

    def gears_from_data(self, data):
        result = []
        for row_number in range(0, len(data)):
            for gear in self.gear_reg.finditer(data[row_number]):
                result.append(Gear(row_number, gear.span()))
        return result

    def part01(self):
        text = super()._part01_input()
        data = [line for line in text.splitlines()]
        numbers = self.numbers_from_data(data)
        total = 0
        for num in numbers:
            for check in num.to_check(data):
                if data[check.row][check.col] != '.':
                    total += num.value
        return total

    def part02(self):
        text = super()._part02_input()
        data = [line for line in text.splitlines()]
        numbers = self.numbers_from_data(data)
        gears = self.gears_from_data(data)
        total = 0
        for gear in gears:
            adj = list(n for n in numbers if len(n.coordinates.intersection(gear.to_check(data))) > 0)
            if len(adj) == 2:
                total += adj[0].value * adj[1].value
        return total

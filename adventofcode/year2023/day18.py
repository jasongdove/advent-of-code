import re

from adventofcode import Day, Coordinate


class Day18(Day):
    def __init__(self):
        super().__init__(2023, 18)
        self.reg = re.compile('([UDLR])\\s(\\d+)')
        self.reg2 = re.compile('.+#(.+)\\)')

    @staticmethod
    def shoelace(points):
        result = 0

        x = [point[0] for point in points] + [points[0][0]]
        y = [point[1] for point in points] + [points[0][1]]

        for i in range(len(points)):
            result += x[i] * y[i + 1] - y[i] * x[i + 1]

        return abs(result) / 2

    def part01(self):
        text = super()._part01_input()
        lines = text.splitlines()
        current = Coordinate(0, 0)
        points = []
        border = 0
        for line in lines:
            matches = self.reg.match(line)
            direction = matches.group(1)
            distance = int(matches.group(2))
            border += distance
            match direction:
                case 'U':
                    current = Coordinate(current.row - distance, current.col)
                case 'D':
                    current = Coordinate(current.row + distance, current.col)
                case 'L':
                    current = Coordinate(current.row, current.col - distance)
                case _:
                    current = Coordinate(current.row, current.col + distance)
            points.append([current.row, current.col])

        return int(Day18.shoelace(points) + (border / 2) + 1)

    def part02(self):
        text = super()._part01_input()
        lines = text.splitlines()
        current = Coordinate(0, 0)
        points = []
        border = 0
        for line in lines:
            matches = self.reg2.match(line)
            h = matches.group(1)
            distance = int(h[:5], 16)
            direction = h[-1]
            border += distance
            match direction:
                case '3':
                    current = Coordinate(current.row - distance, current.col)
                case '1':
                    current = Coordinate(current.row + distance, current.col)
                case '2':
                    current = Coordinate(current.row, current.col - distance)
                case _:
                    current = Coordinate(current.row, current.col + distance)
            points.append([current.row, current.col])

        return int(Day18.shoelace(points) + (border / 2) + 1)

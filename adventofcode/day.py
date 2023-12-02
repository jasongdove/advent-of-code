class Day:
    def __init__(self, year, day):
        self.year = year
        self.day = day

    def _part01_input(self):
        with open(f'resources/{self.year}/day{self.day:02}-part1.txt') as f:
            data = f.read()
        return data

    def _part02_input(self):
        with open(f'resources/{self.year}/day{self.day:02}-part2.txt') as f:
            data = f.read()
        return data

    def part01(self):
        raise NotImplementedError()

    def part02(self):
        raise NotImplementedError()

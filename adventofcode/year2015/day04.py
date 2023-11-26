from adventofcode import Day
from hashlib import md5


class Day04(Day):
    def __init__(self):
        super().__init__(2015, 4)

    def part01(self):
        text = super()._part01_input()
        i = 1
        while True:
            test = md5((text + str(i)).encode()).hexdigest()
            if test.startswith('00000'):
                return i
            else:
                i += 1

    def part02(self):
        text = super()._part01_input()
        i = 1
        while True:
            test = md5((text + str(i)).encode()).hexdigest()
            if test.startswith('000000'):
                return i
            else:
                i += 1

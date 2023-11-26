from adventofcode import Day


class Day03(Day):
    def __init__(self):
        super().__init__(2015, 3)

    @staticmethod
    def get_houses(script):
        x = 0
        y = 0
        houses = {(x, y)}
        for direction in script:
            match direction:
                case '<':
                    x -= 1
                case '>':
                    x += 1
                case '^':
                    y -= 1
                case 'v':
                    y += 1
            houses.add((x, y))
        return houses

    def part01(self):
        text = super()._part01_input()
        return len(Day03.get_houses(text))

    def part02(self):
        text = super()._part01_input()
        one = ""
        two = ""
        for i in range(0, len(text)):
            if i % 2 == 0:
                one += text[i]
            else:
                two += text[i]
        return len(Day03.get_houses(one).union(Day03.get_houses(two)))

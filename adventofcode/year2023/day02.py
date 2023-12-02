from adventofcode import Day
import re


class Game:
    def __init__(self, number, red, green, blue):
        self.number = number
        self.red = red
        self.green = green
        self.blue = blue

    def __str__(self):
        return f'{self.number}: {self.red} R, {self.green} G, {self.blue} B'

    def is_possible(self, red, green, blue):
        return self.red <= red and self.green <= green and self.blue <= blue


class Day02(Day):
    def __init__(self):
        super().__init__(2023, 2)
        self.gn = re.compile('Game (\\d+): (.*)')
        self.clr = re.compile('(\\d+) ([a-z]+)')

    def game_from_line(self, line):
        match = self.gn.match(line)
        number = int(match.group(1))
        rest = match.group(2)
        red = 0
        green = 0
        blue = 0
        for hand in rest.split(';'):
            for color in map(lambda x: x.strip(), hand.split(',')):
                color_match = self.clr.match(color)
                color_count = int(color_match.group(1))
                color_name = color_match.group(2)
                match color_name:
                    case 'red':
                        red = max(red, color_count)
                    case 'green':
                        green = max(green, color_count)
                    case 'blue':
                        blue = max(blue, color_count)
        return Game(number, red, green, blue)

    def part01(self):
        text = super()._part01_input()
        max_red = 12
        max_green = 13
        max_blue = 14
        good_game_numbers = set()
        for line in text.splitlines():
            game = self.game_from_line(line)
            if game.is_possible(max_red, max_green, max_blue):
                good_game_numbers.add(game.number)
        return sum(good_game_numbers)

    def part02(self):
        text = super()._part01_input()
        total = 0
        for line in text.splitlines():
            game = self.game_from_line(line)
            total += game.red * game.green * game.blue
        return total

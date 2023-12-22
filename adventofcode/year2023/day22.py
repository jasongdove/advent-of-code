import re
import heapq

from adventofcode import Day


class Brick:
    def __init__(self, letter: chr, x: int, y: int, z: int, x2: int, y2: int, z2: int):
        self.letter = letter
        self.x = x
        self.y = y
        self.z = z
        self.x2 = x2
        self.y2 = y2
        self.z2 = z2

    def copy(self):
        return Brick(self.letter, self.x, self.y, self.z, self.x2, self.y2, self.z2)

    def contains(self, point: tuple[int, int, int]) -> bool:
        return self.x <= point[0] <= self.x2 and self.y <= point[1] <= self.y2 and self.z <= point[2] <= self.z2

    def points(self, dz: int = 0) -> list[tuple[int, int, int]]:
        result = []
        for x in range(self.x, self.x2 + 1):
            for y in range(self.y, self.y2 + 1):
                for z in range(self.z, self.z2 + 1):
                    result.append((x, y, z + dz))
        return result

    def fall(self, resting_points) -> bool:
        for point in self.points(-1):
            if point[2] == 0 or point in resting_points:
                return False
        self.z -= 1
        self.z2 -= 1
        return True

    def __lt__(self, other):
        min_z = min(self.z, self.z2)
        other_min_z = min(other.z, other.z2)
        return min_z < other_min_z

    def __repr__(self):
        return f'{self.letter}: ({self.x}, {self.y}, {self.z}) => ({self.x2}, {self.y2}, {self.z2})'


class Day22(Day):
    def __init__(self):
        super().__init__(2023, 22)
        self.reg = re.compile(r'(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)')

    def part01(self):
        text = super()._part01_input()
        bricks = []
        letter = 'A'
        for line in text.splitlines():
            match = self.reg.match(line)
            bricks.append(Brick(
                letter,
                int(match.group(1)),
                int(match.group(2)),
                int(match.group(3)),
                int(match.group(4)),
                int(match.group(5)),
                int(match.group(6))))
            letter = chr(ord(letter) + 1)

        falling_bricks = []
        resting_bricks = []
        resting_points = []

        for brick in bricks:
            heapq.heappush(falling_bricks, brick)
            #print(brick)

        while len(falling_bricks) > 0:
            brick = heapq.heappop(falling_bricks)
            if not brick.fall(resting_points):
                resting_bricks.append(brick)
                resting_points.extend(brick.points())
            else:
                heapq.heappush(falling_bricks, brick)

        #print()
        for brick in bricks:
            heapq.heappush(falling_bricks, brick)
            #print(brick)

        total = 0

        for i in range(len(bricks)):
            falling_bricks = []
            resting_points = []
            for j in range(len(bricks)):
                if i != j:
                    heapq.heappush(falling_bricks, bricks[j].copy())
            #print(f'falling_bricks: {list(map(lambda b: b.letter, falling_bricks))}')
            fall = False
            while len(falling_bricks) > 0:
                #print(f'  resting points: {resting_points}')
                brick = heapq.heappop(falling_bricks)
                #print(f'  testing brick: {brick}')
                if not brick.fall(resting_points):
                    #print(f'    not fall')
                    resting_points.extend(brick.points())
                else:
                    #print(f'    fall!')
                    fall = True
                    break
            if not fall:
                total += 1

        return total

    def part02(self):
        text = super()._part01_input()
        return 0

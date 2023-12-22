import re
import heapq

from adventofcode import Day


class Brick:
    def __init__(self, letter: int, x: int, y: int, z: int, x2: int, y2: int, z2: int):
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

    def points(self, dz: int = 0) -> set[tuple[int, int, int]]:
        result = set()
        for x in range(self.x, self.x2 + 1):
            for y in range(self.y, self.y2 + 1):
                result.add((x, y, self.z + dz))
                result.add((x, y, self.z2 + dz))
        return result

    def fall(self, resting_points: set[tuple[int, int, int]]) -> bool:
        result = self.can_fall(resting_points)
        if result:
            self.z -= 1
            self.z2 -= 1
        return result

    def can_fall(self, resting_points: set[tuple[int, int, int]]) -> bool:
        for x in range(self.x, self.x2 + 1):
            for y in range(self.y, self.y2 + 1):
                test_z = self.z - 1
                if test_z == 0 or (x, y, test_z) in resting_points:
                    return False
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
        self.bricks = self.get_bricks()
        self.bricks = self.fall_until_resting(self.bricks)

    def get_bricks(self) -> list[Brick]:
        text = super()._part01_input()
        bricks = []
        count = 1
        for line in text.splitlines():
            match = self.reg.match(line)
            bricks.append(Brick(
                count,
                int(match.group(1)),
                int(match.group(2)),
                int(match.group(3)),
                int(match.group(4)),
                int(match.group(5)),
                int(match.group(6))))
            count = count + 1
        return bricks

    @staticmethod
    def fall_until_resting(bricks: list[Brick]) -> list[Brick]:
        falling_bricks = []
        resting_bricks = []
        resting_points = set()

        for brick in bricks:
            heapq.heappush(falling_bricks, brick)

        while len(falling_bricks) > 0:
            brick = heapq.heappop(falling_bricks)
            if not brick.fall(resting_points):
                resting_bricks.append(brick)
                resting_points.update(brick.points())
            else:
                heapq.heappush(falling_bricks, brick)

        return resting_bricks

    def part01(self):
        total = 0

        for i in range(len(self.bricks)):
            falling_bricks = []
            resting_points = set()
            for j in range(len(self.bricks)):
                if i != j:
                    heapq.heappush(falling_bricks, self.bricks[j])
            fall = False
            while len(falling_bricks) > 0:
                brick = heapq.heappop(falling_bricks)
                if not brick.can_fall(resting_points):
                    resting_points.update(brick.points())
                else:
                    fall = True
                    break
            if not fall:
                total += 1

        return total

    def part02(self):
        total = 0

        for i in range(len(self.bricks)):
            falling_bricks = []
            resting_points = set()
            for j in range(len(self.bricks)):
                if i != j:
                    heapq.heappush(falling_bricks, self.bricks[j].copy())
            fall_status = set()
            while len(falling_bricks) > 0:
                brick = heapq.heappop(falling_bricks)
                if not brick.fall(resting_points):
                    resting_points.update(brick.points())
                else:
                    fall_status.add(brick.letter)
                    heapq.heappush(falling_bricks, brick)
            total += len(fall_status)

        return total

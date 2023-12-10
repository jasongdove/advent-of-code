from adventofcode import Day, Coordinate
import queue


class Day10(Day):
    def __init__(self):
        super().__init__(2023, 10)

    @staticmethod
    def follow(the_map: list[list[chr]], coordinate: Coordinate):
        symbol = the_map[coordinate.row][coordinate.col]
        match symbol:
            case '|':
                return [coordinate.up(), coordinate.down()]
            case '-':
                return [coordinate.left(), coordinate.right()]
            case 'L':
                return [coordinate.up(), coordinate.right()]
            case 'J':
                return [coordinate.up(), coordinate.left()]
            case '7':
                return [coordinate.down(), coordinate.left()]
            case 'F':
                return [coordinate.down(), coordinate.right()]
        return []

    @staticmethod
    def get_connected(the_map: list[list[chr]], coord: Coordinate):
        result = []
        up = coord.up()
        if up.is_valid_in(the_map) and the_map[up.row][up.col] in ['|', '7', 'F']:
            result.append(up)
        down = coord.down()
        if down.is_valid_in(the_map) and the_map[down.row][down.col] in ['|', 'L', 'J']:
            result.append(down)
        left = coord.left()
        if left.is_valid_in(the_map) and the_map[left.row][left.col] in ['-', 'L', 'F']:
            result.append(left)
        right = coord.right()
        if right.is_valid_in(the_map) and the_map[right.row][right.col] in ['-', 'J', '7']:
            result.append(right)
        return result

    @staticmethod
    def bfs(the_map: list[list[chr]]):
        start = Coordinate(0, 0)
        for index, row in enumerate(the_map):
            if 'S' in row:
                start = Coordinate(index, row.index('S'))
        visited = {}
        q = queue.Queue()
        for y in Day10.get_connected(the_map, start):
            q.put_nowait((y, 1))
        while not q.empty():
            n, d = q.get_nowait()
            if n not in visited:
                visited[n] = d
            to_follow = Day10.follow(the_map, n)
            for t in to_follow:
                if t not in visited:
                    q.put_nowait((t, d + 1))
        return visited

    def part01(self):
        text = super()._part01_input()
        the_map = [list(line) for line in text.splitlines()]
        visited = Day10.bfs(the_map)
        return max(visited.values())

    def part02(self):
        text = super()._part02_input()
        the_map = [list(line) for line in text.splitlines()]
        visited = Day10.bfs(the_map)

        # replace start char with implied pipe
        start = Coordinate(0, 0)
        for index, row in enumerate(the_map):
            if 'S' in row:
                start = Coordinate(index, row.index('S'))
        connected_to_start = Day10.get_connected(the_map, start)

        replace = 'S'
        if start.up() in connected_to_start and start.left() in connected_to_start:
            replace = 'J'
        elif start.up() in connected_to_start and start.right() in connected_to_start:
            replace = 'L'
        elif start.up() in connected_to_start and start.down() in connected_to_start:
            replace = '|'
        elif start.down() in connected_to_start and start.left() in connected_to_start:
            replace = '7'
        elif start.down() in connected_to_start and start.right() in connected_to_start:
            replace = 'F'
        elif start.left() in connected_to_start and start.right() in connected_to_start:
            replace = '-'

        the_map[start.row][start.col] = replace

        # count to the right, only including bottom and side edges
        enclosed = []
        for row_index, row in enumerate(the_map):
            for col_index in range(0, len(row)):
                if Coordinate(row_index, col_index) not in visited:
                    right_count = 0
                    for i in range(col_index + 1, len(row)):
                        if Coordinate(row_index, i) in visited and the_map[row_index][i] in ['|', 'J', 'L']:
                            right_count += 1
                    if right_count % 2 == 1:
                        enclosed.append(Coordinate(row_index, col_index))

        return len(enclosed)

from adventofcode import Day
import collections


class Day15(Day):
    def __init__(self):
        super().__init__(2023, 15)

    @staticmethod
    def hash(string: str) -> int:
        current = 0
        for _, c in enumerate(string):
            current += ord(c)
            current *= 17
            current %= 256
        return current

    def part01(self):
        text = super()._part01_input()
        steps = text.replace('\n', '').split(',')
        total = 0
        for step in steps:
            total += Day15.hash(step)
        return total

    def part02(self):
        text = super()._part01_input()
        steps = text.replace('\n', '').split(',')
        boxes: list[collections.deque] = [collections.deque() for _ in range(256)]
        for step in steps:
            if '-' in step:
                label = step.split('-')[0]
                box_for_step = Day15.hash(label)
                for lens in boxes[box_for_step].copy():
                    if lens[0] == label:
                        boxes[box_for_step].remove(lens)
                        break
            elif '=' in step:
                (label, focal_length) = step.split('=')
                box_for_step = Day15.hash(label)
                replaced = False
                for lens in boxes[box_for_step]:
                    if lens[0] == label:
                        lens[1] = int(focal_length)
                        replaced = True
                        break
                if not replaced:
                    boxes[box_for_step].append([label, int(focal_length)])
        total = 0
        for box_index, box in enumerate(boxes):
            for lens_index, lens in enumerate(box):
                total += (box_index + 1) * (lens_index + 1) * lens[1]
        return total

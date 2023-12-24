import re
from itertools import combinations
import z3

from shapely import Point, Polygon
from shapely.geometry import LineString, box

from adventofcode import Day


class Day24(Day):
    def __init__(self):
        super().__init__(2023, 24)
        self.reg = re.compile(r'([-\d]+),\s+([-\d]+),\s+([-\d]+)\s+@\s+([-\d]+),\s+([-\d]+),\s+([-\d]+)')

    @staticmethod
    def extend_line(polygon, l):
        line = LineString([(l[0], l[1]), (l[0] + l[3], l[1] + l[4])])
        minx, miny, maxx, maxy = polygon.bounds
        bounding_box = box(minx, miny, maxx, maxy)
        a, b = line.boundary.geoms
        if a.x == b.x:  # vertical line
            extended_line = LineString([(a.x, miny), (a.x, maxy)])
        elif a.y == b.y:  # horizontal line
            extended_line = LineString([(minx, a.y), (maxx, a.y)])
        else:
            # linear equation: y = k*x + m
            k = (b.y - a.y) / (b.x - a.x)
            m = a.y - k * a.x
            y0 = k * minx + m
            y1 = k * maxx + m
            x0 = (miny - m) / k
            x1 = (maxy - m) / k
            points_on_boundary_lines = [Point(minx, y0), Point(maxx, y1),
                                        Point(x0, miny), Point(x1, maxy)]
            points_sorted_by_distance = sorted(points_on_boundary_lines, key=bounding_box.distance)
            extended_line = LineString(points_sorted_by_distance[:2])
        return extended_line

    @staticmethod
    def do_lines_intersect(combo, lo, hi) -> bool:
        line1 = combo[0][1]
        line2 = combo[1][1]
        intersect: Point = line1.intersection(line2)
        if not intersect.is_empty and lo <= intersect.x <= hi and lo <= intersect.y <= hi:
            ax1 = combo[0][0][0]
            ax2 = ax1 + combo[0][0][3]
            bx1 = combo[1][0][0]
            bx2 = bx1 + combo[1][0][3]
            dax = intersect.x - ax1
            dax2 = intersect.x - ax2
            dbx = intersect.x - bx1
            dbx2 = intersect.x - bx2
            if abs(dax2) > abs(dax) or abs(dbx2) > abs(dbx):
                return False
            return True
        return False

    def part01(self):
        lo = 200000000000000
        hi = 400000000000000
        text = super()._part01_input()
        lines = []
        for text_line in text.splitlines():
            match = self.reg.match(text_line)
            line = (float(match.group(1)), float(match.group(2)), float(match.group(3)),
                    float(match.group(4)), float(match.group(5)), float(match.group(6)))
            lines.append(line)
        total = 0
        polygon = Polygon([(lo, lo), (lo, hi), (hi, lo), (hi, hi)])
        extended_lines = list(map(lambda li: (li, self.extend_line(polygon, li)), lines))
        combo: combinations[tuple[tuple[float, float, float, float, float, float], tuple[LineString, LineString]]]
        for combo in combinations(extended_lines, 2):
            if self.do_lines_intersect(combo, lo, hi):
                total += 1
        return total

    def part02(self):
        text = super()._part01_input()
        lines = []
        for text_line in text.splitlines():
            match = self.reg.match(text_line)
            line = (float(match.group(1)), float(match.group(2)), float(match.group(3)),
                    float(match.group(4)), float(match.group(5)), float(match.group(6)))
            lines.append(line)

        sx, sy, sz, dx, dy, dz = z3.Reals('sx sy sz dx dy dz')
        solver = z3.Solver()
        for i, line in enumerate(lines[:3]):
            ti = z3.Real(f't{i}')
            solver.add(ti > 0)
            solver.add(sx + ti * dx == line[0] + ti * line[3])
            solver.add(sy + ti * dy == line[1] + ti * line[4])
            solver.add(sz + ti * dz == line[2] + ti * line[5])
        solver.check()
        return sum(solver.model()[v].as_long() for v in [sx, sy, sz])

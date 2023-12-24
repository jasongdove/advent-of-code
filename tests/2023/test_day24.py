import unittest
from adventofcode.year2023 import Day24


class TestDay24(unittest.TestCase):

    def setUp(self):
        self.day = Day24()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 16589)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 781390555762385)

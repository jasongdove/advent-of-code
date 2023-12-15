import unittest
from adventofcode.year2023 import Day15


class TestDay15(unittest.TestCase):

    def setUp(self):
        self.day = Day15()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 505459)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 228508)

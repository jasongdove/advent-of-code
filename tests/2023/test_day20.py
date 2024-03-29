import unittest
from adventofcode.year2023 import Day20


class TestDay20(unittest.TestCase):

    def setUp(self):
        self.day = Day20()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 791120136)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 215252378794009)

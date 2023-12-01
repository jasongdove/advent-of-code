import unittest
from adventofcode.year2023 import Day01


class TestDay01(unittest.TestCase):

    def setUp(self):
        self.day = Day01()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 55090)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 54845)

import unittest
from adventofcode.year2015 import Day03


class TestDay02(unittest.TestCase):

    def setUp(self):
        self.day = Day03()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 2592)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 2360)

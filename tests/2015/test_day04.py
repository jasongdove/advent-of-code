import unittest
from adventofcode.year2015 import Day04


class TestDay04(unittest.TestCase):

    def setUp(self):
        self.day = Day04()

    def test_part01(self):
        p1 = self.day.part01()
        self.assertEqual(p1, 282749)

    def test_part02(self):
        p2 = self.day.part02()
        self.assertEqual(p2, 9962624)

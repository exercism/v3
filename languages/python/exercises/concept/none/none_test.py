import unittest
from none import find_the_seat, empty_the_seat


class TestNoneType(unittest.TestCase):

    def test_find_the_seat_1(self):
        self.assertIs(
            find_the_seat([1, None, 3]),
            2,
            msg="the index of the seat which is empty is invalid."
        )

    def test_find_the_seat_2(self):
        self.assertIs(
            find_the_seat([1, 4, 3, None, 7, None]),
            4,
            msg="the index of the seat which is empty is invalid."
        )

    def test_empty_the_seat_1(self):
        self.assertListEqual(
            empty_the_seat([1, 4, 3, 9, 7, 5], 3),
            [1, 4, None, 9, 7, 5],
            msg="The seat is not emptied at the expected seat number"
        )

    def test_empty_the_seat_2(self):
        self.assertListEqual(
            empty_the_seat([1, 4, 3, 9, 7, 5], 6),
            [1, 4, 3, 9, 7, None],
            msg="The seat is not emptied at the expected seat number"
        )

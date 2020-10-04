import unittest
from none import find_the_seat, empty_the_seat, arrange_the_seats, curr_empty_seat_capacity


class TestNoneType(unittest.TestCase):

    def test_empty_the_seat_1(self):
        self.assertDictEqual(
            empty_the_seat({1: "Occupied", 2: None, 3: "Occupied"}, 3),
            {1: "Occupied", 2: None, 3: None},
            msg="The seat is not emptied at the expected seat number"
        )
    
    def test_empty_the_seat_2(self):
        self.assertDictEqual(
            empty_the_seat({1: "Occupied", 2: None, 3: None, 4: "Occupied"}, 4),
            {1: "Occupied", 2: None, 3: None, 4: None},
            msg="The seat is not emptied at the expected seat number"
        )
    
    def test_arrange_the_seats_1(self):
        self.assertDictEqual(
            arrange_the_seats(3),
            {1: None, 2: None, 3: None},
            msg="The seat arranged is not valid"
        )
    
    def test_arrange_the_seats_2(self):
        self.assertDictEqual(
            arrange_the_seats(0),
            {},
            msg="The seat arranged (dict) should be empty"
        )
    
    def test_is_seat_empty_1(self):
        self.assertTrue(
            is_seat_empty(None),
            msg="The Seat is empty. It should be True"
        )
    
    def test_is_seat_empty_2(self):
        self.assertFalse(
            is_seat_empty("Occupied"),
            msg="The Seat is Occupied. It should be False"
        )

    def test_find_the_seat_1(self):
        self.assertIs(
            find_the_seat({1: "Occupied", 2: None, 3: "Occupied"}),
            2,
            msg="the index of the seat which is empty is invalid."
        )

    def test_find_the_seat_2(self):
        self.assertIs(
            find_the_seat({1: "Occupied", 2: "Occupied", 3: None, 4: "Occupied", 5: None}),
            3,
            msg="the index of the seat which is empty is invalid."
        )
    
    def test_curr_seat_cap_1(self):
        self.assertIs(
            curr_empty_seat_capacity({1: "Occupied", 2: None, 3: "Occupied"}),
            1,
            msg="the empty seat count is wrong"
        )

    def test_curr_seat_cap_2(self):
        self.assertIs(
            curr_empty_seat_capacity({1: "Occupied", 2: "Occupied", 3: "Occupied", 4: "Occupied"}),
            0,
            msg="The empty seat count is wrong"
        )

    def test_can_accom_seats_1(self):
        self.assertTrue(
            can_accomodate_seats({1: "Occupied", 2: None, 3: "Occupied"}, 2)
            msg="We can accomadate seats for the input."
        )

    def test_can_accom_seats_2(self):
        self.assertFalse(
            can_accomodate_seats({1: "Occupied", 2: None, 3: "Occupied"}, 1),
            msg="Cannot Accomodate Seats for the Input"
        )
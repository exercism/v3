import unittest
from exchange import *


class TestExchange(unittest.TestCase):
    
    # Problem 1
    def test_estimate_value(self):
        input_data = [
            # input : budget, exchange_rate
            (100000, 0.84),
            (700000, 10.1)
        ]
        output_data = [119047, 69306]
        for input, output in zip(input_data, output_data):
            with self.subTest(input=input, output=output):
                self.assertEqual(int(estimate_value(input[0], input[1])), output)

    # Problem 2
    def test_get_change(self):
        input_data = [
            # input : budget, exchanging_value
            (463000, 5000)
        ]
        output_data = [458000]
        for input, output in zip(input_data, output_data):
            with self.subTest(input=input, output=output):
                self.assertEqual(get_change(input[0], input[1]), output)

    # Problem 3
    def test_get_value(self):
        input_data = [
            # input : denomination_value, number_of_bills
            (10000, 128),
        ]
        output_data = [1280000]
        for input, output in zip(input_data, output_data):
            with self.subTest(input=input, output=output):
                self.assertEqual(get_value(input[0], input[1]), output)

    # Problem 4
    def test_get_number_of_bills(self):
        input_data = [
            # input : budget, denomination
            (163270, 50000),
            (54361, 1000)
        ]
        output_data = [3,54]
        for input, output in zip(input_data, output_data):
            with self.subTest(input=input, output=output):
                self.assertEqual(get_number_of_bills(input[0], input[1]), output)

    # Problem 5
    def test_exchangable_value(self):
        input_data = [
            # input : budget, exchange_rate, spread, minimum_denomination
            (100000, 10.61, 10, 1),
            (1500, 0.84, 25, 40),
            (470000, 1050, 30, 10000000000),
            (470000, 0.00000009, 30, 700),
            (425.33, 0.0009, 30, 700)
        ]
        output_data = [8568, 1400, 0, 4017094016600, 363300]
        for input, output in zip(input_data, output_data):
            with self.subTest(input=input, output=output):
                self.assertEqual(exchangable_value(input[0], input[1], input[2], input[3]), output)

    

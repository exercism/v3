import unittest
from commentpy import omit_String


class TestLogLines(unittest.TestCase):
    def test_message(self):
        self.assertEqual(
            omit_String(),
            "I am Python"
        )

   

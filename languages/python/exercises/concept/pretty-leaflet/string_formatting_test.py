import unittest
import string_formatting


class test_str_formatting(unittest.TestCase):
    def test_todo(self):
        self.assertEqual(capitalize_title("fish are cold blooded"),
                         "Fish Are Cold Blooded")

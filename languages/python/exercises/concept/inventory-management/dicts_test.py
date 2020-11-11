import unittest
from dicts import *


class test_inventory(unittest.TestCase):
    def test_create_inventory(self):
        self.assertEqual(create_inventory(["wood", "iron", "iron", "diamond", "diamond"]),
                         {"wood": 1, "iron": 2, "diamond": 2})

    def test_add_one_item(self):
        self.assertEqual(add_items({"wood": 4, "iron": 2}, ["iron", "iron"]),
                         {"wood": 4, "iron": 4})

    def test_add_multiple_items(self):
        self.assertEqual(add_items({"wood": 2, "gold": 1, "diamond": 3}, ["wood", "gold", "gold"]),
                         {"wood": 3, "gold": 3, "diamond": 3})

    def test_add_new_item(self):
        self.assertEqual(add_items({"iron": 1, "diamond": 2}, ["iron", "wood", "wood"]),
                         {"iron": 2, "diamond": 2, "wood": 2})

    def test_add_from_empty_dict(self):
        self.assertEqual(add_items({}, ["iron", "iron", "diamond"]),
                         {"iron": 2, "diamond": 1})

    def test_delete_items(self):
        self.assertEqual(delete_items({"iron": 3, "diamond": 4, "gold": 2},
                                      ["iron", "iron", "diamond", "gold", "gold"]),
                         {"iron": 1, "diamond": 3, "gold": 0})

    def test_not_below_zero(self):
        self.assertEqual(delete_items({"wood": 2, "iron": 3, "diamond": 1},
                                      ["wood", "wood", "wood", "iron", "diamond", "diamond"]),
                         {"wood": 0, "iron": 2, "diamond": 0})

    def test_list_inventory(self):
        self.assertEqual(list_inventory({"coal": 15, "diamond": 3, "wood": 67}),
                         [("coal", 15), ("diamond", 3), ("wood", 67)])


if __name__ == "__main__":
    unittest.main()

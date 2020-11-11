In this exercise, you will be managing an inventory system.

The inventory should be organized by the item name and it should keep track of the number of items.

You will be given a list of items. Each time an item is in the given list, increase the item's quantity by `1` in the inventory. You will also have to delete items from the inventory.

To finish, you will have to implement a function which returns all the key-value pairs in an inventory as a list of `tuples`.

## 1. Create an inventory based on a list

Implement the `create_inventory()` function that creates an "inventory" from a list of items. It should return a `dictionary` representing the item names associated with their quantity.

```python
>>> create_inventory(["coal", "wood", "wood", "diamond", "diamond", "diamond"])
{"coal":1, "wood":2 "diamond":3}
```

## 2. Add items from a list to an existing dictionary

Implement the `add_items()` function that adds a list of items to an inventory:

```python
>>> add_items({"coal":1}, ["wood", "iron", "coal", "wood"])
{"coal":2, "wood":2, "iron":1}
```

## 3. Remove items from the inventory

Implement the `delete_items()` function that removes every item in the list from an inventory:

```python
>>> delete_items({"coal":3, "diamond":1, "iron":5}, ["diamond", "coal", "iron", "iron"])
{"coal":2, "diamond":0, "iron":3}
```

The item counts should not fall below `0`, if the number of items in the list exceeds the number of items available in the inventory, the quantity for that item should remain `0` and the item should be ignored.

```python
>>> delete_items({"coal":2, "wood":1, "diamond":2}, ["coal", "coal", "wood", "wood", "diamond"])
{"coal":0, "wood":0, "diamond":1}
```

## 4. Return the inventory content

Implement the `list_inventory()` function that takes an inventory and returns a list of `(item, quantity)` tuples. The list only includes the items with a count greater than zero:

```python
>>> list_inventory({"coal":7, "wood":11, "diamond":2, "iron":7, "silver": 0})
[('coal', 7), ('diamond', 2), ('iron', 7), ('wood', 11)]
```

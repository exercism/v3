In this exercise you'll be managing an inventory system.

You will be given a list of items, each time an item is in the given list add `1` to the key in the _given_ inventory. Each item should be organized by their name and the amount of that item. You will also have to delete items from a dictionary.

You will also have to implement a function which returns a list of `tuples` of all the keys in the _given_ inventory.

## 1. Add items from a list to a dictionary

Implement a function that adds a list of items to the inventory:

```python
>>> add_items({"coal":1}, ["wood", "iron", "coal", "wood"])
{"coal":2, "wood":2, "iron":1}
```

## 2. Remove items from a dictionary

Implement a function that removes items from an inventory:

```python
>>> delete_items({"coal":3, "diamond":1, "iron":5}, ["diamond", "coal", "iron", "iron"])
{"coal":2, "diamond":0, "iron":3}
```

## 3. Return the contents of a dictionary

Implement a function that returns a list of tuples with the items and their values:

```python
>>> list_inventory({"coal":7, "wood":11, "diamond":2, "iron":7})
[("coal", 7),("wood", 11),("diamond", 2),("iron", 7)]
```

## 4. Create an inventory from a list

Implement a function that creates an inventory from scratch using a list of items. The function will return a dictionary of the inventory.

```python
>>> create_inventory(["coal", "wood", "wood", "diamond", "diamond", "diamond"])
{"coal":1, "wood":2 "diamond":3}
```
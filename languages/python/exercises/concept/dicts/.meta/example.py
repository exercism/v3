def create_inventory(items):
    inventory = dict()
    add_items(inventory, items)
    return inventory


def add_items(inventory, items):
    for item in items:
        inventory.setdefault(item, 0)
        inventory[item] += 1
    return inventory


def delete_items(inventory, items):
    for item in items:
        inventory.setdefault(item, 0)
        inventory[item] = max(inventory[item] - 1, 0)
    return inventory


def list_inventory(inventory):
    return [(k, v) for k, v in sorted(inventory.items()) if v]

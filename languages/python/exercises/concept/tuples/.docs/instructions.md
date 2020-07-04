Aazra and Rui are teammates competing in a pirate-themed treasure hunt. One has a list of treasures with map coordinates, the other a list of location names with map coordinates. They've also been given blank maps with a starting place marked YOU ARE HERE.

<br>
<table>
<tr><th>Azara's List</th><th></th><th>Rui's List</th></tr>
<tr><td>

| Treasure                    | Coordinates |
| --------------------------- | ----------- |
| Amethyst Octopus            | 1F          |
| Angry Monkey Figurine       | 5B          |
| Antique Glass Fishnet Float | 3D          |
| Brass Spyglass              | 4B          |
| Carved Wooden Elephant      | 8C          |
| Crystal Crab                | 6A          |
| Glass Starfish              | 6D          |
| Model Ship in Large Bottle  | 8A          |
| Pirate Flag                 | 7F          |
| Robot Parrot                | 1C          |
| Scrimshaw Whale's Tooth     | 2A          |
| Silver Seahorse             | 4E          |
| Vintage Pirate Hat          | 7E          |

</td><td></td><td>

| Location Name                         | Coordinates | Quandrant |
| ------------------------------------- | ----------- | --------- |
| Seaside Cottages                      | ("1", "C")  | Blue      |
| Aqua Lagoon (Island of Mystery)       | ("1", "F")  | Yellow    |
| Deserted Docks                        | ("2", "A")  | Blue      |
| Spiky Rocks                           | ("3", "D")  | Yellow    |
| Abandoned Lighthouse                  | ("4", "B")  | Blue      |
| Hidden Spring (Island of Mystery)     | ("4", "E")  | Yellow    |
| Stormy Breakwater                     | ("5", "B")  | Purple    |
| Old Schooner                          | ("6", "A")  | Purple    |
| Tangled Seaweed Patch                 | ("6", "D")  | Orange    |
| Quiet Inlet (Island of Mystery)       | ("7", "E")  | Orange    |
| Windswept Hilltop (Island of Mystery) | ("7", "F")  | Orange    |
| Harbor Managers Office                | ("8", "A")  | Purple    |
| Foggy Seacave                         | ("8", "C")  | Purple    |

</td></tr>
</table>

<br>

But things are a bit disorganized: Azara's coordinates appear to be formatted and sorted differently from Rui's, and they have to keep looking from one list to the other to figure out which treasures go with which locations. Being budding pythonistas, they've come to you for help in writing a small program (a set of functions, really) to better organize their hunt information.

## 1. Extract coordinates.

Given a `(treasure, coordinate)` pair from Azaras list, extract and return the part that represents the coordinate on the map by implementing the `get_cooordinate()` function.

​

```python

>>> get_coordinate(("Scrimshaw Whale's Tooth", "2A"))
>>> "2A"

```

​  
​

## 2. Format coordinates.

Given a coordinate in the format "2A", return a tuple in the format `("2", "A")` by implementing the `convert_coordinate()` function.

​

```python
convert_coordinate("2A")
>>> ("2", "A")

```

## 3. Match coordinates.

Given a `(treasure, coordinate)` pair and a `(location, coordinate, quadrant)` record, compare the coordinates from each, and return **`True`** if they "match", **`False`** if they do not. Re-format the coordinate as needed for an accurate comparison. Implement this as the `compare_records()` function.

```python
compare_records(('Brass Spyglass', '4B'),('Seaside Cottages', ("1", "C"), 'blue'))
>>> False

compare_records(('Model Ship in Large Bottle', '8A'), ('Harbor Managers Office', ("8", "A"), 'purple'))
>>> True
```

## 4. Combine matched records.

Given a `(treasure, coordinate)` pair from Azaras list and a `(location, coordinate, quadrant)` record from Ruis list, **if the coordinates match**, return `(treasure, coordinate, location, coordinate, quadrant)`. Otherwise return "not a match". Re-format the coordinate as needed for accurate comparison. Implement this as the `create_record()` function.

```python
create_record(('Brass Spyglass', '4B'),('Abandoned Lighthouse', ("4", "B"), 'Blue'))
>>> ('Brass Spyglass', '4B', 'Abandoned Lighthouse', ("4", "B"), 'Blue')

create_record(('Brass Spyglass', '4B'),(("1", "C"), 'Seaside Cottages', 'blue'))
>>> "not a match"

```

## 5. "Clean up" & print out all records.

Clean up the combined records from Azara and Rui so that there's only one set of coordinates per record, and there's one list of everything they need to put on their maps.
Given a tuple of tuples, loop through the _outer_ tuple, dropping the unwanted coordinates from each _inner_ tuple and printing each out. Implement ths as the `clean_up()` function.

```python

clean_up((('Brass Spyglass', '4B', 'Abandoned Lighthouse', '("4", "B")', 'Blue'), ('Vintage Pirate Hat', '7E', 'Quiet Inlet (Island of Mystery)', '("7", "E")', 'Orange'), ('Crystal Crab', '6A', 'Old Schooner', '("6", "A")', 'Purple')))

>>> ('Brass Spyglass', 'Abandoned Lighthouse', '("4", "B")', 'Blue')
>>> ('Vintage Pirate Hat', 'Quiet Inlet (Island of Mystery)', '("7", "E")', 'Orange')
>>> ('Crystal Crab', 'Old Schooner', '("6", "A")','Purple')

```

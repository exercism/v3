Your acquaintance need to print thousands of handbill for different events, their need your help. You need to create the layout for a leaflet containing a header, an optional date, and a list of artists each associated with a unicode icon. The goal is to `print` that leaflet and you need to use the string formatting concept to succeed.

## 1. Create the class with a capitalized header

The `Leaflet` class wraps all the instance variables and methods needed to create the leaflet. Implement that class, the header instance variable correspond to the first argument of the constructor.

The constructor should take three parameters: the header of type `str`, one array with a list of artists and one array with a list of unicode points.

The header should be capitalized as illustrated in this example.

```python
>>> Leaflet("title", [], []).header
"Title"
```

## 2. Add an optional date

Implement a new method `set_date(day, month, year)` for the class with an optional value for the parameter year. The values passed in arguments should be formatted as a written date like "December 6, 2021". If this method is not called, the instance variable `date` should be an empty string.

```python
>>> leaflet = Leaflet("title", [], [])
>>> leaflet.date
""
>>> leaflet.set_date(21, 2, 2020)
>>> leaflet.date
"February 21, 2020"
>>> leaflet.set_date(30, 3)
>>> leaflet.date
"March 30, 2020"
```

## 3. Render the unicode points as icons

When the method `get_icons` is called, the list of unicode codes passed as the third argument in the constructor should be rendered and returned as a list of icons.

```python
>>> leaflet = Leaflet("title", [], ['\U00000040', '\U0001D70B'])
>>> leaflet.get_icons()
['@', '𝜋']
```

## 4. Display the finished leaflet

The method `print` should return the finished leaflet, the poster should abide by the layout below.

The leaflet follows a specific layout in the shape of a rectangle:

- The first and last rows contain 20 asterisks. `"*"`
- Each section is separated by an empty row above and below it.
- An empty row contains only one asterisk at the beginning and one at the end.
- The first section is the header of the leaflet, this title is capitalized.
- The second section is the option date, if the date is not defined, this section should be an empty row.
- The third and last section is the list of the artists, each artist associated with the corresponding icon.

```python
********************
*                  *
*    'Concert'     *
*                  *
*  June 22, 2020   *
*                  *
* John         🎸  *
* Benjamin     🎤  *
* Max          🎹  *
*                  *
********************
```

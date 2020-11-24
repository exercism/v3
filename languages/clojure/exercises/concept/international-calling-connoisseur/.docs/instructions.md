In this exercise you'll be writing code to keep track of international dialing codes via an international dialing code hashmap.

The map uses an integer for its keys (the dialing code) and a string (country name) for its values.

You have 8 tasks which involve the exercism.dialing-codes namespace.

### 1. Create a pre-populated hashmap

There exists a pre-populated hashmap which contains the following 3 dialing codes: "United States of America" which has a code of 1, "Brazil" which has a code of 55 and "India" which has a code of 91. Define the `countries` var to store the pre-populated hashmap:

```clojure
countries
;;=> {1 "United States of America", 55 "Brazil", 91 "India"}
```

### 2. Add a country to a map

Implement the `add-country` function that adds a dialing code and associated country name to a hashmap.

```clojure
(add-country countries 44 "United Kingdom")
;;=> {1 "United States of America", 44 "United Kingdom", 55 "Brazil", 91 "India"}
```

### 3. Get the country name matching a country code

Implement the country-name function that takes a dialing code and returns the corresponding country name. If the dialing code is not contained in the map, nil is returned.

```clojure
(country-name countries 55)
;;=> "Brazil"
```

### 4. Update a country name

Implement the `update-map` function which takes a dialing code and replaces the corresponding country name in the map with the country name passed as a parameter. If the dialing code does not exist in the map then the map remains unchanged.

```clojure
(update-map countries 1 "Les États-Unis")
;;=> {1 "Les États-Unis", 55 "Brazil", 91 "India"}
```

### 5. Check that a country exists in the map

Implement the `code-exists?` function to check whether a dialing code exists in the map.

```clojure
(code-exists? countries 55)
;;=> true
```

### 6. Attempt to update name of country that is not in the map

Try to change the name of a country with a code that is not in the map e.g. 999. This should result in no change to the map:

```clojure
(update-map countries 999, "Newlands")
;;=> {1 "United States of America", 44 "United Kingdom", 55 "Brazil", 91 "India"}
```

### 7. Remove a country from the map

Implement the `remove-country` function that takes a dialing code and will remove the corresponding record, dialing code + country name, from the map.

```clojure
(remove-country countries 91)
;;=> {1 "United States of America", 55 "Brazil"}
```

### 8. Find the country with the longest name

Implement the `longest-name` function which will return the name of the country with the longest name stored in the map.

```clojure
(longest-name countries)
;;=> "United States of America"
```

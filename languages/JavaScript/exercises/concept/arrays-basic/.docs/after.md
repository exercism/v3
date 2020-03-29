In Javascript, an array is actually just a regular object where the elements are properties of that object. It includes the `length` property and also has lots of useful methods for traversing and mutating the array.

Accesing elements:

```javascript
const names = ['Jack', 'Laura', 'Paul', 'Megan'];
names[1];
// => Laura
```

Changing elements:

```javascript
const names = ['Jack', 'Laura', 'Paul', 'Megan'];
names[1] = 'Lucy';

names;
// => ['Jack', 'Lucy', 'Paul', 'Megan']
```

Adding element to the end of an array using the `push` method:

```javascript
const names = ['Jack', 'Laura', 'Paul', 'Megan'];
names.push('Bob');

names;
// => ['Jack', 'Laura', 'Paul', 'Megan', 'Bob']
```

Remove element at position using the `splice` method:

```javascript
const names = ['Jack', 'Laura', 'Paul', 'Megan'];
const index = 2;
names.splice(index, 1);

names;
// => ['Jack', 'Laura', 'Megan']
```

Remove last element from an array using the `pop` method:

```javascript
const names = ['Jack', 'Laura', 'Paul', 'Megan'];
names.pop();

names;
// => ['Jack', 'Laura', 'Paul'];
```

Get size of an array using the `length` property:

```javascript
const names = ['Jack', 'Laura', 'Paul', 'Megan'];
names.length;
// => 4;
```

More information about the internals of the array [here][array-docs].

[array-docs]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array

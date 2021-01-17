Type Conversion is often referred when converting the value from one data type to another data type.This is also often referred as "Coercion".

Type conversion may be explicit or implicit.

Explicit type conversion are performed by a function call _meant to convert the type_:

```javascript
const a = 46
String(a)
// => "46"
const b = '3.4'
Number(b)
//=> 3.4
Boolean(b)
// => true
```

Implicit conversion happens as a side-effect of an operation:

```javascript
const a = 10
console.log(`There are ${a} cats in the house`)
// Implicitly converts from Number to String in order to insert the value of a into the template literal.
//=> "There are 10 cats in the house"
```

There are few other method that can be used to convert one type to another such as `array to string`,`object to string` and many more.

```javascript
;['hello', 'world'].join('') // Array to string
//=> "hello world"

false.toString() // any type to string
//=> "false"

const a = new Date()

a.getTime() // Date object to number

const b = { pokemon: 'pikachu' }

const c = JSON.stringify(b) //Object to string
//=> '{"pokemon":"pikachu"}'

JSON.parse(c) //string to object
//=>{pokemon:"pikachu"}
```

There are few ways we can convert one type to Boolean such as using not `!` operator and using `Boolean` object in javascript

```javascript
const a = 'hello'
!!a // string to boolean
// => true
Boolean(a) //using explcit type from string to boolean
//=>true
```

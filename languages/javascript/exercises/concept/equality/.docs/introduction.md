**Equality** in the javascript can be referred in two ways which is `==` or `===`
It is usually a misconception that people consider `==` which is usually check for value equality and `===` which usually check for the equality and type of the comparison which is not true.

The usual description as define in the specification of the javascript is that
`==` allows coercion in the equality while `===` disallow coercion

`==` is usually defined as the abstract equality operator. **This equality prefers Numeric coercion**

It follows certain rules while allowing coercion `x==y`:

- If the type of `x` is same as type of `y`. This Equality return the output of `===`
- If `x` is `null` or `undefined` and `y` is `undefined` or `null`.Comparison return `true`
- If the type of `x` and `y` are not same and one of the type is `number`.The equality coerce it into `number`

`===` is defined as Strict equality operator.If we don't know about the type of the both value.This is operator get used in protect ourself from coercion`

It follows few rules while comparing in `x===y`:

- If the type of `x` and type of `y` is diferent. It return `false`
- If the `x` has the same value as `y`.It returns `true`

**Both the equality only work in comparing the primative types(number,string) not (object,array)**

```javascript
let age = 22
let profile = { name: 'John', age: '22' }
if(Number(age)===Number(profile.age)) // true
if(age==profile.age)// true
//Comparing the abstract equality here will make more sense in this example as we know the age will be number.Abstract Equality prefer numerical coercion
```

```javascript
let profile1={photos:null}
let profile2={};
if((profile1.photos===null || profile1.photos===undefined) && (profile2.photos===null || profile2.photos===undefined))// true;

//Using strict equality comparison here doesn't help to write cleaner code instead we can use
if(profile1.photos==null && profile2.photos==null)// true
```

Avoid using the `==` operator to the non primitive such as object or array.For example

```javascript

let arr=[];
if(arr==false)  // => return true;
//Comparing with boolean will apply the coercion
// Coerce the boolean to number Number(false) =0
// array will convert into primitive as string
// [].toString()=>""
//Number("")=0
//0===0 true
//instead use to boolean like
if(arr)  // return true;
```

Sometimes when we want to compare the value and want to compare the special cases like comparing `0` or `-0`.We can use `Object.is(x,y)` 

`Object.is` does no type conversion and no special handling for NaN, -0, and +0 (giving it the same behavior as === except on those special numeric values).

In Summary we should avoid using `==`:

- `==` with `0`,`""`,or `" "`
- `==` with non-primitives
- `==true` or `==false` instead use toBoolean or use `===`
- In general, If you know the type of both value use `==` else prefer `===`
